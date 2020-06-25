<?xml version="1.0" encoding="UTF-8"?>
<dia:diagram xmlns:dia="http://www.lysator.liu.se/~alla/dia/">
  <dia:dictionarydata>
    <element name="SRYYM" occurs="1">
      <appinfo>
        <embed object="text" length="16"/>
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
    <element name="KAISETUNAME" occurs="1">
      <appinfo>
        <embed object="text" length="40"/>
      </appinfo>
    </element>
    <element name="HOSPCD" occurs="1">
      <appinfo>
        <embed object="text" length="14"/>
      </appinfo>
    </element>
    <element name="TENHYOKBN" occurs="4">
      <appinfo>
        <embed object="text" length="2"/>
      </appinfo>
    </element>
    <element name="DATA-TBL" occurs="27">
      <element name="HKNJANAME" occurs="1">
        <appinfo>
          <embed object="text" length="20"/>
        </appinfo>
      </element>
      <element name="KAMIBUN" occurs="1">
        <appinfo>
          <embed object="text" length="12"/>
        </appinfo>
      </element>
      <element name="NYUGAI-TBL" occurs="2">
        <element name="KENSU" occurs="1">
          <appinfo>
            <embed object="text" length="7"/>
          </appinfo>
        </element>
        <element name="NISSU" occurs="1">
          <appinfo>
            <embed object="text" length="7"/>
          </appinfo>
        </element>
        <element name="TENSU" occurs="1">
          <appinfo>
            <embed object="text" length="14"/>
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
    <dia:object type="Standard - Line" version="0" id="O0">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55,3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,2.9939;19.3344,3.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55,3"/>
        <dia:point val="19.3294,2.9989"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O1">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55,3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,2.995;1.555,27.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55,3"/>
        <dia:point val="1.55,27.65"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O2">
      <dia:attribute name="obj_pos">
        <dia:point val="19.3,3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.295,2.995;19.305,27.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.3,3"/>
        <dia:point val="19.3,27.65"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O3">
      <dia:attribute name="obj_pos">
        <dia:point val="10.5,3.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.495,3.945;19.305,3.955"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.5,3.95"/>
        <dia:point val="19.3,3.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O4">
      <dia:attribute name="obj_pos">
        <dia:point val="13.35,4.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.345,4.745;19.305,4.755"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.35,4.75"/>
        <dia:point val="19.3,4.75"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O5">
      <dia:attribute name="obj_pos">
        <dia:point val="13.35,6.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.345,6.345;19.305,6.355"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.35,6.35"/>
        <dia:point val="19.3,6.35"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O6">
      <dia:attribute name="obj_pos">
        <dia:point val="10.5,5.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.495,5.545;19.305,5.555"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.5,5.55"/>
        <dia:point val="19.3,5.55"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O7">
      <dia:attribute name="obj_pos">
        <dia:point val="13.35,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.345,7.895;19.305,7.905"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.35,7.9"/>
        <dia:point val="19.3,7.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O8">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55,8.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,8.695;19.305,8.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55,8.7"/>
        <dia:point val="19.3,8.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O9">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55,10.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,10.2449;19.2938,10.255"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55,10.25"/>
        <dia:point val="19.2888,10.2499"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O10">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55,11.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,11.845;19.3043,11.8563"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55,11.85"/>
        <dia:point val="19.2993,11.8513"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O11">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55,13.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,13.445;19.305,13.455"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55,13.45"/>
        <dia:point val="19.3,13.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O12">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55,15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,14.995;19.305,15.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55,15"/>
        <dia:point val="19.3,15"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O13">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55,16.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,16.595;19.305,16.605"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55,16.6"/>
        <dia:point val="19.3,16.6"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O14">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55,18.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,18.145;19.3041,18.1558"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55,18.15"/>
        <dia:point val="19.2991,18.1508"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O15">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55,19.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,19.745;19.305,19.755"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55,19.75"/>
        <dia:point val="19.3,19.75"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O16">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55,21.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,21.345;19.305,21.355"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55,21.35"/>
        <dia:point val="19.3,21.35"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O17">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55,22.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,22.895;19.305,22.905"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55,22.9"/>
        <dia:point val="19.3,22.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O18">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55,24.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,24.495;19.305,24.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55,24.5"/>
        <dia:point val="19.3,24.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O19">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55,26.075"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,26.07;10.4389,26.08"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55,26.075"/>
        <dia:point val="10.4339,26.075"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O20">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4927,3.00313"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.4877,2.99813;10.4977,27.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.4927,3.00313"/>
        <dia:point val="10.4927,27.65"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O21">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4397,2.99945"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.4268,2.99445;10.4447,27.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.4397,2.99945"/>
        <dia:point val="10.4318,27.65"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O0" connection="0"/>
        <dia:connection handle="1" to="O40" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O22">
      <dia:attribute name="obj_pos">
        <dia:point val="13.3495,3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.3445,2.995;13.355,27.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.3495,3"/>
        <dia:point val="13.35,27.65"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O23">
      <dia:attribute name="obj_pos">
        <dia:point val="13.75,3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.745,2.995;13.755,27.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.75,3"/>
        <dia:point val="13.75,27.65"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O24">
      <dia:attribute name="obj_pos">
        <dia:point val="15.25,3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.245,2.995;15.255,27.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.25,3"/>
        <dia:point val="15.25,27.65"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O25">
      <dia:attribute name="obj_pos">
        <dia:point val="16.7,3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.695,2.995;16.705,27.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="16.7,3"/>
        <dia:point val="16.7,27.65"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O26">
      <dia:attribute name="obj_pos">
        <dia:point val="4.4506,7.74543"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.445,7.74043;4.4556,27.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.4506,7.74543"/>
        <dia:point val="4.45,27.65"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O27">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55833,7.74742"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.55333,7.74242;10.455,7.755"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55833,7.74742"/>
        <dia:point val="10.45,7.75"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O28">
      <dia:attribute name="obj_pos">
        <dia:point val="13.35,9.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.345,9.495;19.305,9.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.35,9.5"/>
        <dia:point val="19.3,9.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O29">
      <dia:attribute name="obj_pos">
        <dia:point val="13.35,11.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.345,11.045;19.305,11.055"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.35,11.05"/>
        <dia:point val="19.3,11.05"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O30">
      <dia:attribute name="obj_pos">
        <dia:point val="13.35,12.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.345,12.645;19.305,12.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.35,12.65"/>
        <dia:point val="19.3,12.65"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O31">
      <dia:attribute name="obj_pos">
        <dia:point val="13.35,14.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.345,14.195;19.305,14.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.35,14.2"/>
        <dia:point val="19.3,14.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O32">
      <dia:attribute name="obj_pos">
        <dia:point val="13.3495,15.7969"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.3445,15.7919;19.305,15.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.3495,15.7969"/>
        <dia:point val="19.3,15.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O33">
      <dia:attribute name="obj_pos">
        <dia:point val="13.35,17.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.345,17.395;19.305,17.405"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.35,17.4"/>
        <dia:point val="19.3,17.4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O34">
      <dia:attribute name="obj_pos">
        <dia:point val="13.35,18.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.345,18.945;19.305,18.955"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.35,18.95"/>
        <dia:point val="19.3,18.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O35">
      <dia:attribute name="obj_pos">
        <dia:point val="13.35,20.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.345,20.545;19.305,20.555"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.35,20.55"/>
        <dia:point val="19.3,20.55"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O36">
      <dia:attribute name="obj_pos">
        <dia:point val="13.35,22.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.345,22.095;19.305,22.105"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.35,22.1"/>
        <dia:point val="19.3,22.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O37">
      <dia:attribute name="obj_pos">
        <dia:point val="13.35,23.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.345,23.695;19.305,23.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.35,23.7"/>
        <dia:point val="19.3,23.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O38">
      <dia:attribute name="obj_pos">
        <dia:point val="13.35,25.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.345,25.295;19.305,25.305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.35,25.3"/>
        <dia:point val="19.3,25.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O39">
      <dia:attribute name="obj_pos">
        <dia:point val="13.35,26.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.345,26.845;19.305,26.855"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.35,26.85"/>
        <dia:point val="19.3,26.85"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O40">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55,27.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,27.645;19.3186,27.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55,27.65"/>
        <dia:point val="19.3136,27.65"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O41">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45,9.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.445,9.445;10.455,9.455"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.45,9.45"/>
        <dia:point val="10.45,9.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O42">
      <dia:attribute name="obj_pos">
        <dia:point val="10.5,7.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.495,7.095;19.305,7.105"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.5,7.1"/>
        <dia:point val="19.3,7.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O43">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55,7.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,7.695;10.455,7.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55,7.7"/>
        <dia:point val="10.45,7.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O44">
      <dia:attribute name="obj_pos">
        <dia:point val="4.85048,7.7404"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.845,7.7354;4.85548,27.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.85048,7.7404"/>
        <dia:point val="4.85,27.65"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O45">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55,4.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,4.245;10.455,4.255"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55,4.25"/>
        <dia:point val="10.45,4.25"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O46">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45,11.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.445,11.045;10.455,11.055"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.45,11.05"/>
        <dia:point val="10.45,11.05"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O47">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45,12.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.445,12.645;10.455,12.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.45,12.65"/>
        <dia:point val="10.45,12.65"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O48">
      <dia:attribute name="obj_pos">
        <dia:point val="4.436,14.2008"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.431,14.195;10.455,14.2058"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.436,14.2008"/>
        <dia:point val="10.45,14.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O49">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45,15.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.445,15.845;10.455,15.855"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.45,15.85"/>
        <dia:point val="10.45,15.85"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O50">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45,17.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.445,17.395;10.455,17.405"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.45,17.4"/>
        <dia:point val="10.45,17.4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O51">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45,18.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.445,18.945;10.455,18.955"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.45,18.95"/>
        <dia:point val="10.45,18.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O52">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45,20.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.445,20.545;10.455,20.555"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.45,20.55"/>
        <dia:point val="10.45,20.55"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O53">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45,22.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.445,22.095;10.455,22.105"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.45,22.1"/>
        <dia:point val="10.45,22.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O54">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45,23.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.445,23.695;10.455,23.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.45,23.7"/>
        <dia:point val="10.45,23.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O55">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45368,25.2511"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.44868,25.245;10.455,25.2561"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.45368,25.2511"/>
        <dia:point val="10.45,25.25"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O56">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45,26.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.445,26.845;10.455,26.855"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.45,26.85"/>
        <dia:point val="10.45,26.85"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O57">
      <dia:attribute name="obj_pos">
        <dia:point val="4.5,3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.495,2.995;4.505,4.255"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.5,3"/>
        <dia:point val="4.5,4.25"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O58">
      <dia:attribute name="obj_pos">
        <dia:point val="6.2,7.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.195,7.745;6.205,27.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6.2,7.75"/>
        <dia:point val="6.2,27.65"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O59">
      <dia:attribute name="obj_pos">
        <dia:point val="7.8,7.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.795,7.745;7.805,27.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="7.8,7.75"/>
        <dia:point val="7.8,27.65"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O60">
      <dia:attribute name="obj_pos">
        <dia:point val="16.7248,1.07774"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6998,1.05274;19.3156,1.84479"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="16.7248,1.07774"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="2.5657899999999998"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="0.74204899999999996"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.050000000000000003"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="true"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O61">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4923,26.075"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.4873,26.07;19.3058,26.08"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.4923,26.075"/>
        <dia:point val="19.3008,26.075"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O62">
      <dia:attribute name="obj_pos">
        <dia:point val="10.5027,26.125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.4977,26.12;19.3102,26.1338"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.5027,26.125"/>
        <dia:point val="19.3052,26.1288"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
  </dia:layer>
  <dia:layer name="文字" visible="true">
    <dia:object type="Standard - Text" version="1" id="O63">
      <dia:attribute name="obj_pos">
        <dia:point val="7.61287,2.58944"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.61287,2.14944;18.1129,2.65194"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#診療（調剤）報酬・訪問看護療養費請求総括票#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.61287,2.58944"/>
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
    <dia:object type="Standard - Text" version="1" id="O64">
      <dia:attribute name="obj_pos">
        <dia:point val="2.06012,3.76691"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.06012,3.45691;4.16012,3.80941"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#機関等コード#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.06012,3.76691"/>
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
    <dia:object type="Standard - Text" version="1" id="O65">
      <dia:attribute name="obj_pos">
        <dia:point val="1.64691,4.6268"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.64691,4.3618;7.64691,4.6643"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#保険医療機関・薬局・訪問看護ステーション#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.64691,4.6268"/>
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
    <dia:object type="Standard - Text" version="1" id="O66">
      <dia:attribute name="obj_pos">
        <dia:point val="5.06688,8.35666"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.06688,8.04666;6.11688,8.39916"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件　数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.06688,8.35666"/>
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
    <dia:object type="Standard - Text" version="1" id="O67">
      <dia:attribute name="obj_pos">
        <dia:point val="6.4295,8.32611"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.4295,8.06111;7.6295,8.36361"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#日(回)数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4295,8.32611"/>
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
    <dia:object type="Standard - Text" version="1" id="O68">
      <dia:attribute name="obj_pos">
        <dia:point val="8.36274,8.34771"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.36274,8.03771;9.93774,8.39021"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点   　数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.36274,8.34771"/>
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
    <dia:object type="Standard - Text" version="1" id="O69">
      <dia:attribute name="obj_pos">
        <dia:point val="11.709,26.9285"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.709,26.6185;12.059,26.971"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#計#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.709,26.9285"/>
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
    <dia:object type="Standard - Text" version="1" id="O70">
      <dia:attribute name="obj_pos">
        <dia:point val="1.64691,5.4209"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.64691,5.1559;3.14691,5.4584"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#開設者氏名#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.64691,5.4209"/>
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
    <dia:object type="Standard - Text" version="1" id="O71">
      <dia:attribute name="obj_pos">
        <dia:point val="1.58571,28.0646"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.58571,27.7996;9.68571,28.1021"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※県内分と県外分はそれぞれ別の用紙に記載してください。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.58571,28.0646"/>
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
    <dia:object type="Standard - Text" version="1" id="O72">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,8.98625"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,8.76625;4.8,9.33125"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,8.98625"/>
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
    <dia:object type="Standard - Text" version="1" id="O73">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,9.6872"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,9.4672;4.8,10.3447"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,9.6872"/>
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
    <dia:object type="Standard - Text" version="1" id="O74">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,10.5759"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,10.3559;4.8,10.9209"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,10.5759"/>
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
    <dia:object type="Standard - Text" version="1" id="O75">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,11.2768"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,11.0568;4.8,11.9343"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,11.2768"/>
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
    <dia:object type="Standard - Text" version="1" id="O76">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,12.1525"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,11.9325;4.8,12.4975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,12.1525"/>
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
    <dia:object type="Standard - Text" version="1" id="O77">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,12.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,12.68;4.8,13.5575"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,12.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O78">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,13.7723"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,13.5523;4.8,14.1173"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,13.7723"/>
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
    <dia:object type="Standard - Text" version="1" id="O79">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,14.4732"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,14.2532;4.8,15.1307"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,14.4732"/>
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
    <dia:object type="Standard - Text" version="1" id="O80">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,15.3619"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,15.1419;4.8,15.7069"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,15.3619"/>
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
    <dia:object type="Standard - Text" version="1" id="O81">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,16.0629"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,15.8429;4.8,16.7204"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,16.0629"/>
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
    <dia:object type="Standard - Text" version="1" id="O82">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,16.9385"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,16.7185;4.8,17.2835"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,16.9385"/>
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
    <dia:object type="Standard - Text" version="1" id="O83">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,17.6395"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,17.4195;4.8,18.297"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,17.6395"/>
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
    <dia:object type="Standard - Text" version="1" id="O84">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,18.4913"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,18.2713;4.8,18.8363"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,18.4913"/>
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
    <dia:object type="Standard - Text" version="1" id="O85">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,19.1923"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,18.9723;4.8,19.8498"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,19.1923"/>
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
    <dia:object type="Standard - Text" version="1" id="O86">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,20.0809"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,19.8609;4.8,20.4259"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,20.0809"/>
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
    <dia:object type="Standard - Text" version="1" id="O87">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,20.7819"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,20.5619;4.8,21.4394"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,20.7819"/>
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
    <dia:object type="Standard - Text" version="1" id="O88">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,21.6576"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,21.4376;4.8,22.0026"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,21.6576"/>
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
    <dia:object type="Standard - Text" version="1" id="O89">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,22.3585"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,22.1385;4.8,23.016"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,22.3585"/>
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
    <dia:object type="Standard - Text" version="1" id="O90">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,23.2125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,22.9925;4.8,23.5575"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,23.2125"/>
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
    <dia:object type="Standard - Text" version="1" id="O91">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,23.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,23.73;4.8,24.6075"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,23.95"/>
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
    <dia:object type="Standard - Text" version="1" id="O92">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,24.8021"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,24.5821;4.8,25.1471"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,24.8021"/>
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
    <dia:object type="Standard - Text" version="1" id="O93">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,25.5031"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,25.2831;4.8,26.1606"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,25.5031"/>
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
    <dia:object type="Standard - Text" version="1" id="O94">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,26.3787"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,26.1587;4.8,26.7237"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,26.3787"/>
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
    <dia:object type="Standard - Text" version="1" id="O95">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,27.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,26.88;4.8,27.7575"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55,27.1"/>
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
    <dia:object type="Standard - Text" version="1" id="O96">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,4.28538"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,4.06538;13.7,4.63038"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,4.28538"/>
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
    <dia:object type="Standard - Text" version="1" id="O97">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,4.98632"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,4.76632;13.7,5.64382"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,4.98632"/>
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
    <dia:object type="Standard - Text" version="1" id="O98">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,5.87501"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,5.65501;13.7,6.22001"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,5.87501"/>
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
    <dia:object type="Standard - Text" version="1" id="O99">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,6.57595"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,6.35595;13.7,7.23345"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,6.57595"/>
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
    <dia:object type="Standard - Text" version="1" id="O100">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,7.45161"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,7.23161;13.7,7.79661"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,7.45161"/>
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
    <dia:object type="Standard - Text" version="1" id="O101">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,8.15256"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,7.93256;13.7,8.81006"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,8.15256"/>
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
    <dia:object type="Standard - Text" version="1" id="O102">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,9.07141"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,8.85141;13.7,9.41641"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,9.07141"/>
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
    <dia:object type="Standard - Text" version="1" id="O103">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,9.71466"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,9.49466;13.7,10.3722"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,9.71466"/>
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
    <dia:object type="Standard - Text" version="1" id="O104">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,10.661"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,10.441;13.7,11.006"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,10.661"/>
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
    <dia:object type="Standard - Text" version="1" id="O105">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,11.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,11.08;13.7,11.9575"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,11.3"/>
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
    <dia:object type="Standard - Text" version="1" id="O106">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,12.2377"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,12.0177;13.7,12.5827"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,12.2377"/>
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
    <dia:object type="Standard - Text" version="1" id="O107">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,12.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,12.68;13.7,13.5575"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,12.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O108">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,13.7904"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,13.5704;13.7,14.1354"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,13.7904"/>
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
    <dia:object type="Standard - Text" version="1" id="O109">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,14.4914"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,14.2714;13.7,15.1489"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,14.4914"/>
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
    <dia:object type="Standard - Text" version="1" id="O110">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,15.3801"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,15.1601;13.7,15.7251"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,15.3801"/>
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
        <dia:point val="13.45,16.081"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,15.861;13.7,16.7385"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,16.081"/>
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
        <dia:point val="13.45,16.9567"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,16.7367;13.7,17.3017"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,16.9567"/>
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
        <dia:point val="13.45,17.6576"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,17.4376;13.7,18.3151"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,17.6576"/>
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
        <dia:point val="13.45,18.5116"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,18.2916;13.7,18.8566"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,18.5116"/>
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
        <dia:point val="13.45,19.2126"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,18.9926;13.7,19.8701"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,19.2126"/>
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
    <dia:object type="Standard - Text" version="1" id="O116">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,20.1013"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,19.8813;13.7,20.4463"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,20.1013"/>
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
    <dia:object type="Standard - Text" version="1" id="O117">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,20.8022"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,20.5822;13.7,21.4597"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,20.8022"/>
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
    <dia:object type="Standard - Text" version="1" id="O118">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,21.6779"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,21.4579;13.7,22.0229"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,21.6779"/>
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
    <dia:object type="Standard - Text" version="1" id="O119">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,22.3788"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,22.1588;13.7,23.0363"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,22.3788"/>
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
    <dia:object type="Standard - Text" version="1" id="O120">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,23.2219"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,23.0019;13.7,23.5669"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,23.2219"/>
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
    <dia:object type="Standard - Text" version="1" id="O121">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,23.9553"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,23.7353;13.7,24.6128"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,23.9553"/>
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
    <dia:object type="Standard - Text" version="1" id="O122">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,24.8656"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,24.6456;13.7,25.2106"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,24.8656"/>
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
    <dia:object type="Standard - Text" version="1" id="O123">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,25.5449"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,25.3249;13.7,26.2024"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,25.5449"/>
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
    <dia:object type="Standard - Text" version="1" id="O124">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,26.4314"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,26.2114;13.7,26.7764"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,26.4314"/>
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
    <dia:object type="Standard - Text" version="1" id="O125">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,27.0862"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,26.8662;13.7,27.7437"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,27.0862"/>
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
    <dia:object type="Standard - Text" version="1" id="O126">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55594,8.19866"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55594,7.97866;4.80594,8.54366"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#区
分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.55594,8.19866"/>
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
    <dia:object type="Standard - Text" version="1" id="O127">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2787,8.33512"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2787,8.02512;4.0287,8.37762"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#広域連合名#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2787,8.33512"/>
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
    <dia:object type="Standard - Text" version="1" id="O128">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0465,3.64147"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0465,3.33147;15.0965,3.68397"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件　数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0465,3.64147"/>
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
    <dia:object type="Standard - Text" version="1" id="O129">
      <dia:attribute name="obj_pos">
        <dia:point val="15.4091,3.61093"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.4091,3.34593;16.6091,3.64843"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#日(回)数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.4091,3.61093"/>
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
    <dia:object type="Standard - Text" version="1" id="O130">
      <dia:attribute name="obj_pos">
        <dia:point val="17.2344,3.61093"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.2344,3.30093;18.8094,3.65343"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点   　数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.2344,3.61093"/>
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
    <dia:object type="Standard - Text" version="1" id="O131">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4484,3.44314"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.4484,3.22314;13.6984,3.78814"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#区
分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4484,3.44314"/>
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
    <dia:object type="Standard - Text" version="1" id="O132">
      <dia:attribute name="obj_pos">
        <dia:point val="11.1423,3.5921"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.1423,3.2821;12.8923,3.6346"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#広域連合名#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.1423,3.5921"/>
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
    <dia:object type="Standard - Text" version="1" id="O133">
      <dia:attribute name="obj_pos">
        <dia:point val="17.2944,1.64771"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.2944,1.16271;18.6694,1.71521"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#後 期#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.6875"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.2944,1.64771"/>
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
    <dia:object type="Standard - Text" version="1" id="O134">
      <dia:attribute name="obj_pos">
        <dia:point val="9.89371,6.97388"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.89371,6.66388;10.2437,7.01638"/>
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
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.89371,6.97388"/>
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
    <dia:object type="Standard - Text" version="1" id="O135">
      <dia:attribute name="obj_pos">
        <dia:point val="4.85159,2.62252"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.85159,2.18252;5.35159,2.68502"/>
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
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.85159,2.62252"/>
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
    <dia:object type="Standard - Text" version="1" id="O136">
      <dia:attribute name="obj_pos">
        <dia:point val="1.70671,1.7038"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.70671,1.3063;5.75671,1.7588"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#【後期高齢者医療】#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.70671,1.7038"/>
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
    <dia:object type="Standard - Text" version="1" id="O137">
      <dia:attribute name="obj_pos">
        <dia:point val="1.64691,5.04278"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.64691,4.77778;4.04691,5.08028"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#の所在地及び名称#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.64691,5.04278"/>
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
    <dia:object type="Embed - Text" version="1" id="O138">
      <dia:attribute name="obj_pos">
        <dia:point val="1.86651,11.1156"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86651,10.9031;4.26651,11.1456"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.86651,11.1156"/>
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
        <dia:string>#DATA-TBL[1].HKNJANAME#</dia:string>
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
        <dia:point val="1.86651,12.778"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86651,12.5655;4.26651,12.808"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.86651,12.778"/>
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
        <dia:string>#DATA-TBL[2].HKNJANAME#</dia:string>
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
        <dia:point val="1.86651,14.4208"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86651,14.2083;4.26651,14.4508"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.86651,14.4208"/>
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
        <dia:string>#DATA-TBL[3].HKNJANAME#</dia:string>
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
        <dia:point val="1.86651,15.9076"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86651,15.6951;4.26651,15.9376"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.86651,15.9076"/>
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
        <dia:string>#DATA-TBL[4].HKNJANAME#</dia:string>
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
        <dia:point val="1.84883,17.5296"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.84883,17.3171;4.24883,17.5596"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.84883,17.5296"/>
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
        <dia:string>#DATA-TBL[5].HKNJANAME#</dia:string>
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
        <dia:point val="1.86651,19.1114"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86651,18.8989;4.26651,19.1414"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.86651,19.1114"/>
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
        <dia:string>#DATA-TBL[6].HKNJANAME#</dia:string>
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
        <dia:point val="1.89751,20.657"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.89751,20.4445;4.29751,20.687"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.89751,20.657"/>
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
        <dia:string>#DATA-TBL[7].HKNJANAME#</dia:string>
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
        <dia:point val="1.92852,22.2331"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.92852,22.0206;4.32852,22.2631"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.92852,22.2331"/>
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
        <dia:string>#DATA-TBL[8].HKNJANAME#</dia:string>
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
        <dia:point val="1.86651,23.8027"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86651,23.5902;4.26651,23.8327"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.86651,23.8027"/>
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
        <dia:string>#DATA-TBL[9].HKNJANAME#</dia:string>
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
        <dia:point val="1.86651,25.3554"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86651,25.1429;4.26651,25.3854"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.86651,25.3554"/>
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
        <dia:string>#DATA-TBL[10].HKNJANAME#</dia:string>
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
        <dia:point val="1.86651,26.9357"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86651,26.7232;4.26651,26.9657"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.86651,26.9357"/>
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
        <dia:string>#DATA-TBL[11].HKNJANAME#</dia:string>
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
        <dia:point val="1.86651,9.67763"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86651,9.46513;4.26651,9.70763"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.86651,9.67763"/>
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
        <dia:string>#DATA-TBL[0].HKNJANAME#</dia:string>
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
        <dia:point val="4.80459,2.6137"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.80459,2.1737;4.80459,2.6762"/>
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
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.80459,2.6137"/>
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
        <dia:string>#SRYYM#</dia:string>
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
        <dia:point val="4.06282,5.3472"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.06282,5.1272;10.3128,5.6922"/>
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
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.06282,5.3472"/>
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
    <dia:object type="Embed - Text" version="1" id="O152">
      <dia:attribute name="obj_pos">
        <dia:point val="4.04516,6.10073"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.04516,5.88073;9.04516,6.44573"/>
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
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.04516,6.10073"/>
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
    <dia:object type="Embed - Text" version="1" id="O153">
      <dia:attribute name="obj_pos">
        <dia:point val="4.0409,6.92933"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.0409,6.68183;9.6409,6.96433"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.34999999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.0409,6.92933"/>
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
        <dia:string>#KAISETUNAME#</dia:string>
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
        <dia:point val="10.7473,6.41976"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,6.20726;13.1473,6.44976"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7473,6.41976"/>
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
        <dia:string>#DATA-TBL[13].HKNJANAME#</dia:string>
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
        <dia:point val="10.7473,8.10783"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,7.89533;13.1473,8.13783"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7473,8.10783"/>
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
        <dia:string>#DATA-TBL[14].HKNJANAME#</dia:string>
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
        <dia:point val="10.7473,9.67763"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,9.46513;13.1473,9.70763"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7473,9.67763"/>
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
        <dia:string>#DATA-TBL[15].HKNJANAME#</dia:string>
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
        <dia:point val="10.7473,11.1156"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,10.9031;13.1473,11.1456"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7473,11.1156"/>
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
        <dia:string>#DATA-TBL[16].HKNJANAME#</dia:string>
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
        <dia:point val="10.7473,12.778"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,12.5655;13.1473,12.808"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7473,12.778"/>
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
        <dia:string>#DATA-TBL[17].HKNJANAME#</dia:string>
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
        <dia:point val="10.7473,14.4208"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,14.2083;13.1473,14.4508"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7473,14.4208"/>
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
        <dia:string>#DATA-TBL[18].HKNJANAME#</dia:string>
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
        <dia:point val="10.7783,15.9076"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7783,15.6951;13.1783,15.9376"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7783,15.9076"/>
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
        <dia:string>#DATA-TBL[19].HKNJANAME#</dia:string>
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
        <dia:point val="10.7916,17.5296"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7916,17.3171;13.1916,17.5596"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7916,17.5296"/>
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
        <dia:string>#DATA-TBL[20].HKNJANAME#</dia:string>
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
        <dia:point val="10.7473,19.1114"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,18.8989;13.1473,19.1414"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7473,19.1114"/>
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
        <dia:string>#DATA-TBL[21].HKNJANAME#</dia:string>
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
        <dia:point val="10.7473,20.657"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,20.4445;13.1473,20.687"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7473,20.657"/>
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
        <dia:string>#DATA-TBL[22].HKNJANAME#</dia:string>
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
        <dia:point val="10.7473,22.2508"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,22.0383;13.1473,22.2808"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7473,22.2508"/>
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
        <dia:string>#DATA-TBL[23].HKNJANAME#</dia:string>
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
        <dia:point val="10.7473,5.02295"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,4.81045;13.1473,5.05295"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7473,5.02295"/>
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
        <dia:string>#DATA-TBL[12].HKNJANAME#</dia:string>
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
        <dia:point val="10.8093,23.8204"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.8093,23.6079;13.2093,23.8504"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.8093,23.8204"/>
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
        <dia:string>#DATA-TBL[24].HKNJANAME#</dia:string>
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
        <dia:point val="10.8093,25.3554"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.8093,25.1429;13.2093,25.3854"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.8093,25.3554"/>
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
        <dia:string>#DATA-TBL[25].HKNJANAME#</dia:string>
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
        <dia:point val="5.95,3.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.95,3.27;8.05,3.8725"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.75"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.95,3.8"/>
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
    <dia:object type="Standard - Ellipse" version="0" id="O169">
      <dia:attribute name="obj_pos">
        <dia:point val="9.84775,6.63378"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.84275,6.62878;10.2947,7.08072"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="9.84775,6.63378"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="0.441942"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="0.441942"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O170">
      <dia:attribute name="obj_pos">
        <dia:point val="10.8,26.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.8,26.2875;13.2,26.53"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.8,26.5"/>
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
        <dia:string>#DATA-TBL[26].HKNJANAME#</dia:string>
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
        <dia:point val="10.313,9.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,8.935;10.313,9.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,9.2"/>
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
        <dia:string>#DATA-TBL[0].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="6.14105,9.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,8.935;6.14105,9.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,9.2"/>
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
        <dia:string>#DATA-TBL[0].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="7.66605,9.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,8.935;7.66605,9.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,9.2"/>
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
        <dia:string>#DATA-TBL[0].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="1.87471,10.1152"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,9.9027;3.31471,10.1452"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.87471,10.1152"/>
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
        <dia:string>#DATA-TBL[0].KAMIBUN#</dia:string>
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
        <dia:point val="1.87471,11.5862"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,11.3737;3.31471,11.6162"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.87471,11.5862"/>
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
        <dia:string>#DATA-TBL[1].KAMIBUN#</dia:string>
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
        <dia:point val="1.87471,13.2362"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,13.0237;3.31471,13.2662"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.87471,13.2362"/>
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
        <dia:string>#DATA-TBL[2].KAMIBUN#</dia:string>
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
        <dia:point val="1.87471,14.7862"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,14.5737;3.31471,14.8162"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.87471,14.7862"/>
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
        <dia:string>#DATA-TBL[3].KAMIBUN#</dia:string>
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
        <dia:point val="1.87471,16.3862"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,16.1737;3.31471,16.4162"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.87471,16.3862"/>
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
        <dia:string>#DATA-TBL[4].KAMIBUN#</dia:string>
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
        <dia:point val="1.87471,17.9112"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,17.6987;3.31471,17.9412"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.87471,17.9112"/>
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
        <dia:string>#DATA-TBL[5].KAMIBUN#</dia:string>
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
        <dia:point val="1.87471,19.4862"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,19.2737;3.31471,19.5162"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.87471,19.4862"/>
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
        <dia:string>#DATA-TBL[6].KAMIBUN#</dia:string>
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
        <dia:point val="1.87471,21.0612"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,20.8487;3.31471,21.0912"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.87471,21.0612"/>
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
        <dia:string>#DATA-TBL[7].KAMIBUN#</dia:string>
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
        <dia:point val="1.87471,22.6711"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,22.4586;3.31471,22.7011"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.87471,22.6711"/>
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
        <dia:string>#DATA-TBL[8].KAMIBUN#</dia:string>
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
        <dia:point val="1.87471,24.2461"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,24.0336;3.31471,24.2761"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.87471,24.2461"/>
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
        <dia:string>#DATA-TBL[9].KAMIBUN#</dia:string>
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
        <dia:point val="1.87471,25.7211"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,25.5086;3.31471,25.7511"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.87471,25.7211"/>
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
        <dia:string>#DATA-TBL[10].KAMIBUN#</dia:string>
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
        <dia:point val="1.87471,27.3711"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,27.1586;3.31471,27.4011"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.87471,27.3711"/>
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
        <dia:string>#DATA-TBL[11].KAMIBUN#</dia:string>
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
        <dia:point val="10.7589,5.3708"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,5.1583;12.1989,5.4008"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7589,5.3708"/>
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
        <dia:string>#DATA-TBL[12].KAMIBUN#</dia:string>
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
        <dia:point val="10.7589,6.85475"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,6.64225;12.1989,6.88475"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7589,6.85475"/>
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
        <dia:string>#DATA-TBL[13].KAMIBUN#</dia:string>
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
        <dia:point val="10.7589,8.48454"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,8.27204;12.1989,8.51454"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7589,8.48454"/>
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
        <dia:string>#DATA-TBL[14].KAMIBUN#</dia:string>
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
        <dia:point val="10.7589,10.0531"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,9.8406;12.1989,10.0831"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7589,10.0531"/>
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
        <dia:string>#DATA-TBL[15].KAMIBUN#</dia:string>
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
        <dia:point val="10.7589,11.5281"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,11.3156;12.1989,11.5581"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7589,11.5281"/>
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
        <dia:string>#DATA-TBL[16].KAMIBUN#</dia:string>
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
        <dia:point val="10.7589,13.2531"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,13.0406;12.1989,13.2831"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7589,13.2531"/>
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
        <dia:string>#DATA-TBL[17].KAMIBUN#</dia:string>
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
        <dia:point val="10.7589,14.8096"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,14.5971;12.1989,14.8396"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7589,14.8096"/>
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
        <dia:string>#DATA-TBL[18].KAMIBUN#</dia:string>
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
        <dia:point val="10.7589,16.3346"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,16.1221;12.1989,16.3646"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7589,16.3346"/>
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
        <dia:string>#DATA-TBL[19].KAMIBUN#</dia:string>
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
        <dia:point val="10.7589,17.9096"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,17.6971;12.1989,17.9396"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7589,17.9096"/>
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
        <dia:string>#DATA-TBL[20].KAMIBUN#</dia:string>
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
        <dia:point val="10.7589,19.5461"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,19.3336;12.1989,19.5761"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7589,19.5461"/>
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
        <dia:string>#DATA-TBL[21].KAMIBUN#</dia:string>
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
        <dia:point val="10.7589,21.0961"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,20.8836;12.1989,21.1261"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7589,21.0961"/>
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
        <dia:string>#DATA-TBL[22].KAMIBUN#</dia:string>
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
        <dia:point val="10.7589,22.6961"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,22.4836;12.1989,22.7261"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7589,22.6961"/>
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
        <dia:string>#DATA-TBL[23].KAMIBUN#</dia:string>
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
        <dia:point val="10.7589,24.2711"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,24.0586;12.1989,24.3011"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7589,24.2711"/>
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
        <dia:string>#DATA-TBL[24].KAMIBUN#</dia:string>
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
        <dia:point val="10.7589,25.7461"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,25.5336;12.1989,25.7761"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7589,25.7461"/>
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
        <dia:string>#DATA-TBL[25].KAMIBUN#</dia:string>
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
        <dia:point val="10.7589,27.2711"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,27.0586;12.1989,27.3011"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7589,27.2711"/>
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
        <dia:string>#DATA-TBL[26].KAMIBUN#</dia:string>
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
        <dia:point val="10.313,9.9562"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,9.6912;10.313,9.9937"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,9.9562"/>
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
        <dia:string>#DATA-TBL[0].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="6.14105,9.9562"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,9.6912;6.14105,9.9937"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,9.9562"/>
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
        <dia:string>#DATA-TBL[0].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="7.66605,9.9562"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,9.6912;7.66605,9.9937"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,9.9562"/>
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
        <dia:string>#DATA-TBL[0].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="10.313,10.8043"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,10.5393;10.313,10.8418"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,10.8043"/>
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
        <dia:string>#DATA-TBL[1].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="6.14105,10.8043"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,10.5393;6.14105,10.8418"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,10.8043"/>
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
        <dia:string>#DATA-TBL[1].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="7.66605,10.8043"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,10.5393;7.66605,10.8418"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,10.8043"/>
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
        <dia:string>#DATA-TBL[1].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="10.313,11.5605"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,11.2955;10.313,11.598"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,11.5605"/>
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
        <dia:string>#DATA-TBL[1].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="6.14105,11.5605"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,11.2955;6.14105,11.598"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,11.5605"/>
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
        <dia:string>#DATA-TBL[1].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="7.66605,11.5605"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,11.2955;7.66605,11.598"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,11.5605"/>
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
        <dia:string>#DATA-TBL[1].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="10.313,12.4297"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,12.1647;10.313,12.4672"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,12.4297"/>
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
        <dia:string>#DATA-TBL[2].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="6.14105,12.4297"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,12.1647;6.14105,12.4672"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,12.4297"/>
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
        <dia:string>#DATA-TBL[2].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="7.66605,12.4297"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,12.1647;7.66605,12.4672"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,12.4297"/>
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
        <dia:string>#DATA-TBL[2].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="10.313,13.1859"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,12.9209;10.313,13.2234"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,13.1859"/>
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
        <dia:string>#DATA-TBL[2].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="6.14105,13.1859"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,12.9209;6.14105,13.2234"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,13.1859"/>
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
        <dia:string>#DATA-TBL[2].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="7.66605,13.1859"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,12.9209;7.66605,13.2234"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,13.1859"/>
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
        <dia:string>#DATA-TBL[2].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="10.313,13.9845"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,13.7195;10.313,14.022"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,13.9845"/>
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
        <dia:string>#DATA-TBL[3].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="6.14105,13.9845"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,13.7195;6.14105,14.022"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,13.9845"/>
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
        <dia:string>#DATA-TBL[3].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="7.66605,13.9845"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,13.7195;7.66605,14.022"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,13.9845"/>
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
        <dia:string>#DATA-TBL[3].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="10.313,14.7407"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,14.4757;10.313,14.7782"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,14.7407"/>
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
        <dia:string>#DATA-TBL[3].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="6.14105,14.7407"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,14.4757;6.14105,14.7782"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,14.7407"/>
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
        <dia:string>#DATA-TBL[3].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="7.66605,14.7407"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,14.4757;7.66605,14.7782"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,14.7407"/>
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
        <dia:string>#DATA-TBL[3].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="10.313,15.5569"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,15.2919;10.313,15.5944"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,15.5569"/>
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
        <dia:string>#DATA-TBL[4].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="6.14105,15.5569"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,15.2919;6.14105,15.5944"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,15.5569"/>
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
        <dia:string>#DATA-TBL[4].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="7.66605,15.5569"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,15.2919;7.66605,15.5944"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,15.5569"/>
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
        <dia:string>#DATA-TBL[4].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="10.313,16.3131"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,16.0481;10.313,16.3506"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,16.3131"/>
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
        <dia:string>#DATA-TBL[4].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="6.14105,16.3131"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,16.0481;6.14105,16.3506"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,16.3131"/>
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
        <dia:string>#DATA-TBL[4].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="7.66605,16.3131"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,16.0481;7.66605,16.3506"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,16.3131"/>
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
        <dia:string>#DATA-TBL[4].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="10.313,17.1647"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,16.8997;10.313,17.2022"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,17.1647"/>
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
        <dia:string>#DATA-TBL[5].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="6.14105,17.1647"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,16.8997;6.14105,17.2022"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,17.1647"/>
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
        <dia:string>#DATA-TBL[5].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="7.66605,17.1647"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,16.8997;7.66605,17.2022"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,17.1647"/>
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
        <dia:string>#DATA-TBL[5].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="10.313,17.9209"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,17.6559;10.313,17.9584"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,17.9209"/>
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
        <dia:string>#DATA-TBL[5].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="6.14105,17.9209"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,17.6559;6.14105,17.9584"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,17.9209"/>
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
        <dia:string>#DATA-TBL[5].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="7.66605,17.9209"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,17.6559;7.66605,17.9584"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,17.9209"/>
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
        <dia:string>#DATA-TBL[5].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="10.313,18.6841"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,18.4191;10.313,18.7216"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,18.6841"/>
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
        <dia:string>#DATA-TBL[6].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="6.14105,18.6841"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,18.4191;6.14105,18.7216"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,18.6841"/>
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
        <dia:string>#DATA-TBL[6].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="7.66605,18.6841"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,18.4191;7.66605,18.7216"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,18.6841"/>
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
        <dia:string>#DATA-TBL[6].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="10.313,19.4403"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,19.1753;10.313,19.4778"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,19.4403"/>
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
        <dia:string>#DATA-TBL[6].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="6.14105,19.4403"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,19.1753;6.14105,19.4778"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,19.4403"/>
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
        <dia:string>#DATA-TBL[6].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="7.66605,19.4403"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,19.1753;7.66605,19.4778"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,19.4403"/>
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
        <dia:string>#DATA-TBL[6].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="10.3065,20.2919"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.2065,20.0269;10.3065,20.3294"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3065,20.2919"/>
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
        <dia:string>#DATA-TBL[7].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="6.14105,20.2919"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,20.0269;6.14105,20.3294"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,20.2919"/>
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
        <dia:string>#DATA-TBL[7].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="7.66605,20.2919"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,20.0269;7.66605,20.3294"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,20.2919"/>
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
        <dia:string>#DATA-TBL[7].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="10.313,21.0481"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,20.7831;10.313,21.0856"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,21.0481"/>
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
        <dia:string>#DATA-TBL[7].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="6.14105,21.0481"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,20.7831;6.14105,21.0856"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,21.0481"/>
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
        <dia:string>#DATA-TBL[7].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="7.66605,21.0481"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,20.7831;7.66605,21.0856"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,21.0481"/>
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
        <dia:string>#DATA-TBL[7].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="10.313,21.8467"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,21.5817;10.313,21.8842"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,21.8467"/>
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
        <dia:string>#DATA-TBL[8].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="6.14105,21.8467"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,21.5817;6.14105,21.8842"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,21.8467"/>
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
        <dia:string>#DATA-TBL[8].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="7.66605,21.8467"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,21.5817;7.66605,21.8842"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,21.8467"/>
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
        <dia:string>#DATA-TBL[8].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="10.313,22.6029"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,22.3379;10.313,22.6404"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,22.6029"/>
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
        <dia:string>#DATA-TBL[8].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="6.14105,22.6029"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,22.3379;6.14105,22.6404"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,22.6029"/>
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
        <dia:string>#DATA-TBL[8].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="7.66605,22.6029"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,22.3379;7.66605,22.6404"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,22.6029"/>
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
        <dia:string>#DATA-TBL[8].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="10.313,23.4368"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,23.1718;10.313,23.4743"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,23.4368"/>
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
        <dia:string>#DATA-TBL[9].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="6.14105,23.4368"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,23.1718;6.14105,23.4743"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,23.4368"/>
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
        <dia:string>#DATA-TBL[9].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="7.66605,23.4368"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,23.1718;7.66605,23.4743"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,23.4368"/>
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
        <dia:string>#DATA-TBL[9].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="10.313,24.193"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,23.928;10.313,24.2305"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,24.193"/>
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
        <dia:string>#DATA-TBL[9].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="6.14105,24.193"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,23.928;6.14105,24.2305"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,24.193"/>
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
        <dia:string>#DATA-TBL[9].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="7.66605,24.193"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,23.928;7.66605,24.2305"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,24.193"/>
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
        <dia:string>#DATA-TBL[9].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="10.313,25.0449"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,24.7799;10.313,25.0824"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,25.0449"/>
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
        <dia:string>#DATA-TBL[10].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="6.14105,25.0449"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,24.7799;6.14105,25.0824"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,25.0449"/>
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
        <dia:string>#DATA-TBL[10].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="7.66605,25.0449"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,24.7799;7.66605,25.0824"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,25.0449"/>
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
        <dia:string>#DATA-TBL[10].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="10.313,25.8011"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,25.5361;10.313,25.8386"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,25.8011"/>
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
        <dia:string>#DATA-TBL[10].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="6.14105,25.8011"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,25.5361;6.14105,25.8386"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,25.8011"/>
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
        <dia:string>#DATA-TBL[10].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="7.66605,25.8011"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,25.5361;7.66605,25.8386"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,25.8011"/>
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
        <dia:string>#DATA-TBL[10].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="10.313,26.635"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,26.37;10.313,26.6725"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,26.635"/>
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
        <dia:string>#DATA-TBL[11].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="6.14105,26.635"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,26.37;6.14105,26.6725"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,26.635"/>
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
        <dia:string>#DATA-TBL[11].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="7.66605,26.635"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,26.37;7.66605,26.6725"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,26.635"/>
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
        <dia:string>#DATA-TBL[11].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="10.313,27.3912"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.213,27.1262;10.313,27.4287"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.313,27.3912"/>
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
        <dia:string>#DATA-TBL[11].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="6.14105,27.3912"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09105,27.1262;6.14105,27.4287"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.14105,27.3912"/>
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
        <dia:string>#DATA-TBL[11].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="7.66605,27.3912"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61605,27.1262;7.66605,27.4287"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66605,27.3912"/>
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
        <dia:string>#DATA-TBL[11].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="19.2313,4.49684"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,4.23184;19.2313,4.53434"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,4.49684"/>
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
        <dia:string>#DATA-TBL[12].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="15.0843,4.49684"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,4.23184;15.0843,4.53434"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,4.49684"/>
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
        <dia:string>#DATA-TBL[12].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="16.5843,4.49684"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,4.23184;16.5843,4.53434"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,4.49684"/>
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
        <dia:string>#DATA-TBL[12].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="19.2313,5.25304"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,4.98804;19.2313,5.29054"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,5.25304"/>
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
        <dia:string>#DATA-TBL[12].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="15.0843,5.25304"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,4.98804;15.0843,5.29054"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,5.25304"/>
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
        <dia:string>#DATA-TBL[12].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="16.5843,5.25304"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,4.98804;16.5843,5.29054"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,5.25304"/>
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
        <dia:string>#DATA-TBL[12].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="19.2313,6.10461"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,5.83961;19.2313,6.14211"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,6.10461"/>
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
        <dia:string>#DATA-TBL[13].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="15.0843,6.10461"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,5.83961;15.0843,6.14211"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,6.10461"/>
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
        <dia:string>#DATA-TBL[13].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="16.5843,6.10461"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,5.83961;16.5843,6.14211"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,6.10461"/>
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
        <dia:string>#DATA-TBL[13].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="19.2313,6.86082"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,6.59582;19.2313,6.89832"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,6.86082"/>
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
        <dia:string>#DATA-TBL[13].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="15.0843,6.86082"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,6.59582;15.0843,6.89832"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,6.86082"/>
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
        <dia:string>#DATA-TBL[13].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="16.5843,6.86082"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,6.59582;16.5843,6.89832"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,6.86082"/>
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
        <dia:string>#DATA-TBL[13].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="19.2313,7.62405"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,7.35905;19.2313,7.66155"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,7.62405"/>
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
        <dia:string>#DATA-TBL[14].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="15.0843,7.62405"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,7.35905;15.0843,7.66155"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,7.62405"/>
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
        <dia:string>#DATA-TBL[14].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="16.5843,7.62405"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,7.35905;16.5843,7.66155"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,7.62405"/>
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
        <dia:string>#DATA-TBL[14].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="19.2313,8.38025"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,8.11525;19.2313,8.41775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,8.38025"/>
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
        <dia:string>#DATA-TBL[14].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="15.0843,8.38025"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,8.11525;15.0843,8.41775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,8.38025"/>
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
        <dia:string>#DATA-TBL[14].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="16.5843,8.38025"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,8.11525;16.5843,8.41775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,8.38025"/>
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
        <dia:string>#DATA-TBL[14].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="19.2313,9.23182"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,8.96682;19.2313,9.26932"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,9.23182"/>
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
        <dia:string>#DATA-TBL[15].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="15.0843,9.23182"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,8.96682;15.0843,9.26932"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,9.23182"/>
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
        <dia:string>#DATA-TBL[15].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="16.5843,9.23182"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,8.96682;16.5843,9.26932"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,9.23182"/>
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
        <dia:string>#DATA-TBL[15].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="19.2313,9.98803"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,9.72303;19.2313,10.0255"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,9.98803"/>
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
        <dia:string>#DATA-TBL[15].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="15.0843,9.98803"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,9.72303;15.0843,10.0255"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,9.98803"/>
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
        <dia:string>#DATA-TBL[15].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="16.5843,9.98803"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,9.72303;16.5843,10.0255"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,9.98803"/>
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
        <dia:string>#DATA-TBL[15].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="19.2313,10.8043"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,10.5393;19.2313,10.8418"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,10.8043"/>
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
        <dia:string>#DATA-TBL[16].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="15.0843,10.8043"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,10.5393;15.0843,10.8418"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,10.8043"/>
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
        <dia:string>#DATA-TBL[16].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="16.5843,10.8043"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,10.5393;16.5843,10.8418"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,10.8043"/>
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
        <dia:string>#DATA-TBL[16].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="19.2313,11.5605"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,11.2955;19.2313,11.598"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,11.5605"/>
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
        <dia:string>#DATA-TBL[16].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="15.0843,11.5605"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,11.2955;15.0843,11.598"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,11.5605"/>
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
        <dia:string>#DATA-TBL[16].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="16.5843,11.5605"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,11.2955;16.5843,11.598"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,11.5605"/>
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
        <dia:string>#DATA-TBL[16].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="19.2313,12.3944"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,12.1294;19.2313,12.4319"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,12.3944"/>
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
        <dia:string>#DATA-TBL[17].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="15.0843,12.3944"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,12.1294;15.0843,12.4319"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,12.3944"/>
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
        <dia:string>#DATA-TBL[17].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="16.5843,12.3944"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,12.1294;16.5843,12.4319"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,12.3944"/>
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
        <dia:string>#DATA-TBL[17].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="19.2313,13.1506"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,12.8856;19.2313,13.1881"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,13.1506"/>
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
        <dia:string>#DATA-TBL[17].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="15.0843,13.1506"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,12.8856;15.0843,13.1881"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,13.1506"/>
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
        <dia:string>#DATA-TBL[17].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="16.5843,13.1506"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,12.8856;16.5843,13.1881"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,13.1506"/>
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
        <dia:string>#DATA-TBL[17].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="19.2313,13.9668"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,13.7018;19.2313,14.0043"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,13.9668"/>
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
        <dia:string>#DATA-TBL[18].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="15.0843,13.9668"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,13.7018;15.0843,14.0043"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,13.9668"/>
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
        <dia:string>#DATA-TBL[18].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="16.5843,13.9668"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,13.7018;16.5843,14.0043"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,13.9668"/>
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
        <dia:string>#DATA-TBL[18].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="19.2313,14.723"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,14.458;19.2313,14.7605"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,14.723"/>
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
        <dia:string>#DATA-TBL[18].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="15.0843,14.723"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,14.458;15.0843,14.7605"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,14.723"/>
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
        <dia:string>#DATA-TBL[18].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="16.5843,14.723"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,14.458;16.5843,14.7605"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,14.723"/>
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
        <dia:string>#DATA-TBL[18].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="19.2313,15.5569"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,15.2919;19.2313,15.5944"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,15.5569"/>
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
        <dia:string>#DATA-TBL[19].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="15.0843,15.5569"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,15.2919;15.0843,15.5944"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,15.5569"/>
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
        <dia:string>#DATA-TBL[19].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="16.5843,15.5569"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,15.2919;16.5843,15.5944"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,15.5569"/>
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
        <dia:string>#DATA-TBL[19].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="19.2313,16.3131"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,16.0481;19.2313,16.3506"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,16.3131"/>
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
        <dia:string>#DATA-TBL[19].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="15.0843,16.3131"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,16.0481;15.0843,16.3506"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,16.3131"/>
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
        <dia:string>#DATA-TBL[19].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="16.5843,16.3131"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,16.0481;16.5843,16.3506"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,16.3131"/>
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
        <dia:string>#DATA-TBL[19].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="19.2313,17.147"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,16.882;19.2313,17.1845"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,17.147"/>
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
        <dia:string>#DATA-TBL[20].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="15.0843,17.147"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,16.882;15.0843,17.1845"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,17.147"/>
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
        <dia:string>#DATA-TBL[20].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="16.5843,17.147"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,16.882;16.5843,17.1845"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,17.147"/>
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
        <dia:string>#DATA-TBL[20].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="19.2313,17.9032"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,17.6382;19.2313,17.9407"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,17.9032"/>
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
        <dia:string>#DATA-TBL[20].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="15.0843,17.9032"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,17.6382;15.0843,17.9407"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,17.9032"/>
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
        <dia:string>#DATA-TBL[20].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="16.5843,17.9032"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,17.6382;16.5843,17.9407"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,17.9032"/>
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
        <dia:string>#DATA-TBL[20].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="19.2313,18.7195"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,18.4545;19.2313,18.757"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,18.7195"/>
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
        <dia:string>#DATA-TBL[21].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="15.0843,18.7195"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,18.4545;15.0843,18.757"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,18.7195"/>
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
        <dia:string>#DATA-TBL[21].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="16.5843,18.7195"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,18.4545;16.5843,18.757"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,18.7195"/>
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
        <dia:string>#DATA-TBL[21].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="19.2313,19.4757"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,19.2107;19.2313,19.5132"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,19.4757"/>
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
        <dia:string>#DATA-TBL[21].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="15.0843,19.4757"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,19.2107;15.0843,19.5132"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,19.4757"/>
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
        <dia:string>#DATA-TBL[21].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="16.5843,19.4757"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,19.2107;16.5843,19.5132"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,19.4757"/>
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
        <dia:string>#DATA-TBL[21].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="19.2313,20.2742"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,20.0092;19.2313,20.3117"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,20.2742"/>
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
        <dia:string>#DATA-TBL[22].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="15.0843,20.2742"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,20.0092;15.0843,20.3117"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,20.2742"/>
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
        <dia:string>#DATA-TBL[22].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="16.5843,20.2742"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,20.0092;16.5843,20.3117"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,20.2742"/>
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
        <dia:string>#DATA-TBL[22].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="19.2313,21.0304"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,20.7654;19.2313,21.0679"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,21.0304"/>
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
        <dia:string>#DATA-TBL[22].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="15.0843,21.0304"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,20.7654;15.0843,21.0679"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,21.0304"/>
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
        <dia:string>#DATA-TBL[22].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="16.5843,21.0304"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,20.7654;16.5843,21.0679"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,21.0304"/>
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
        <dia:string>#DATA-TBL[22].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="19.2313,21.8824"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,21.6174;19.2313,21.9199"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,21.8824"/>
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
        <dia:string>#DATA-TBL[23].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="15.0843,21.8824"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,21.6174;15.0843,21.9199"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,21.8824"/>
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
        <dia:string>#DATA-TBL[23].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="16.5843,21.8824"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,21.6174;16.5843,21.9199"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,21.8824"/>
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
        <dia:string>#DATA-TBL[23].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="19.2313,22.6386"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,22.3736;19.2313,22.6761"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,22.6386"/>
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
        <dia:string>#DATA-TBL[23].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="15.0843,22.6386"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,22.3736;15.0843,22.6761"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,22.6386"/>
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
        <dia:string>#DATA-TBL[23].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="16.5843,22.6386"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,22.3736;16.5843,22.6761"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,22.6386"/>
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
        <dia:string>#DATA-TBL[23].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="19.2313,23.4548"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,23.1898;19.2313,23.4923"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,23.4548"/>
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
        <dia:string>#DATA-TBL[24].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="15.0843,23.4548"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,23.1898;15.0843,23.4923"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,23.4548"/>
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
        <dia:string>#DATA-TBL[24].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="16.5843,23.4548"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,23.1898;16.5843,23.4923"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,23.4548"/>
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
        <dia:string>#DATA-TBL[24].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="19.2313,24.211"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,23.946;19.2313,24.2485"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,24.211"/>
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
        <dia:string>#DATA-TBL[24].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="15.0843,24.211"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,23.946;15.0843,24.2485"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,24.211"/>
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
        <dia:string>#DATA-TBL[24].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="16.5843,24.211"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,23.946;16.5843,24.2485"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,24.211"/>
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
        <dia:string>#DATA-TBL[24].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="19.2313,25.0802"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,24.8152;19.2313,25.1177"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,25.0802"/>
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
        <dia:string>#DATA-TBL[25].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="15.0843,25.0802"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,24.8152;15.0843,25.1177"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,25.0802"/>
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
        <dia:string>#DATA-TBL[25].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="16.5843,25.0802"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,24.8152;16.5843,25.1177"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,25.0802"/>
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
        <dia:string>#DATA-TBL[25].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="19.2313,25.8364"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,25.5714;19.2313,25.8739"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,25.8364"/>
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
        <dia:string>#DATA-TBL[25].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="15.0843,25.8364"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,25.5714;15.0843,25.8739"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,25.8364"/>
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
        <dia:string>#DATA-TBL[25].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="16.5843,25.8364"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,25.5714;16.5843,25.8739"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,25.8364"/>
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
        <dia:string>#DATA-TBL[25].NYUGAI-TBL[1].NISSU#</dia:string>
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
        <dia:point val="19.2313,26.6173"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,26.3523;19.2313,26.6548"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,26.6173"/>
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
        <dia:string>#DATA-TBL[26].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="15.0843,26.6173"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,26.3523;15.0843,26.6548"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,26.6173"/>
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
        <dia:string>#DATA-TBL[26].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="16.5843,26.6173"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,26.3523;16.5843,26.6548"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,26.6173"/>
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
        <dia:string>#DATA-TBL[26].NYUGAI-TBL[0].NISSU#</dia:string>
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
        <dia:point val="19.2313,27.3559"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1313,27.0909;19.2313,27.3934"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2313,27.3559"/>
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
        <dia:string>#DATA-TBL[26].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="15.0843,27.3735"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0343,27.1085;15.0843,27.411"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0843,27.3735"/>
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
        <dia:string>#DATA-TBL[26].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="16.5843,27.3735"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5343,27.1085;16.5843,27.411"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.5843,27.3735"/>
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
        <dia:string>#DATA-TBL[26].NYUGAI-TBL[1].NISSU#</dia:string>
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
