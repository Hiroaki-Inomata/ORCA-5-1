<?xml version="1.0" encoding="UTF-8"?>
<dia:diagram xmlns:dia="http://www.lysator.liu.se/~alla/dia/">
  <dia:dictionarydata>
    <dia:element name="SRYYM" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="16"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="HOSPCD" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="20"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="ADRS" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="100"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="HOSPNAME" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="100"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="TEL" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="15"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="SRYKA" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="4"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="SEIKYU" occurs="40">
      <dia:element name="CITYNUM" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="6"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="CITYNAME" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="20"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="KENSU" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="5"/>
        </dia:appinfo>
      </dia:element>
    </dia:element>
    <dia:element name="GKENSU" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="6"/>
      </dia:appinfo>
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
  <dia:layer name="線" visible="true">
    <dia:object type="Standard - Box" version="0" id="O0">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,7.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,7.395;3.205,8.455"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="1.5,7.4"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="1.7"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="1.05"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O1">
      <dia:attribute name="obj_pos">
        <dia:point val="1.55,9.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,9.295;19.505,27.555"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="1.55,9.3"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="17.95"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="18.25"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O2">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,5.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,5.645;9.505,6.805"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="1.5,5.65"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="8"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="1.15"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O3">
      <dia:attribute name="obj_pos">
        <dia:point val="1.5,6.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,6.795;1.505,7.405"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.5,6.8"/>
        <dia:point val="1.5,7.4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O2" connection="5"/>
        <dia:connection handle="1" to="O0" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O4">
      <dia:attribute name="obj_pos">
        <dia:point val="2.3,5.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.295,5.645;2.305,6.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.3,5.65"/>
        <dia:point val="2.3,6.8"/>
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
        <dia:point val="3.1,5.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.095,5.645;3.105,6.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.1,5.65"/>
        <dia:point val="3.1,6.8"/>
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
        <dia:point val="3.9,5.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.895,5.645;3.905,6.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.9,5.65"/>
        <dia:point val="3.9,6.8"/>
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
        <dia:point val="4.7,4.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.695,4.795;4.705,7.405"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.7,4.8"/>
        <dia:point val="4.7,7.4"/>
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
        <dia:point val="5.5,5.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.495,5.645;5.505,6.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.5,5.65"/>
        <dia:point val="5.5,6.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O2" connection="1"/>
        <dia:connection handle="1" to="O2" connection="6"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O9">
      <dia:attribute name="obj_pos">
        <dia:point val="6.3,5.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.295,5.645;6.305,6.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6.3,5.65"/>
        <dia:point val="6.3,6.8"/>
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
        <dia:point val="7.1,5.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.095,5.645;7.105,6.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="7.1,5.65"/>
        <dia:point val="7.1,6.8"/>
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
        <dia:point val="8.7,5.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.695,5.645;8.705,6.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.7,5.65"/>
        <dia:point val="8.7,6.8"/>
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
        <dia:point val="1.5,4.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,4.795;1.505,5.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.5,4.8"/>
        <dia:point val="1.5,5.65"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="1" to="O2" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O13">
      <dia:attribute name="obj_pos">
        <dia:point val="4.7,7.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,7.395;4.705,7.405"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.7,7.4"/>
        <dia:point val="3.2,7.4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="1" to="O0" connection="2"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O14">
      <dia:attribute name="obj_pos">
        <dia:point val="4.7,4.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.495,4.795;4.705,4.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.7,4.8"/>
        <dia:point val="1.5,4.8"/>
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
        <dia:point val="7.9,5.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.895,5.645;7.905,6.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="7.9,5.65"/>
        <dia:point val="7.9,6.8"/>
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
        <dia:point val="7.4,3.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,3.445;7.405,3.455"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="7.4,3.45"/>
        <dia:point val="1.55,3.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O17">
      <dia:attribute name="obj_pos">
        <dia:point val="11,3.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.995,3.095;14.705,3.755"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="11,3.1"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="3.7"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="0.65"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O18">
      <dia:attribute name="obj_pos">
        <dia:point val="11,3.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.995,3.745;19.555,4.655"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="11,3.75"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="8.55"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="0.9"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O19">
      <dia:attribute name="obj_pos">
        <dia:point val="11,4.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.995,4.645;14.755,5.305"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="11,4.65"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="3.75"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="0.65"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O20">
      <dia:attribute name="obj_pos">
        <dia:point val="11,5.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.995,5.295;19.555,6.205"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="11,5.3"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="8.55"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="0.9"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O21">
      <dia:attribute name="obj_pos">
        <dia:point val="11,6.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.995,6.195;14.755,6.905"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="11,6.2"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="3.75"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="0.7"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O22">
      <dia:attribute name="obj_pos">
        <dia:point val="11,6.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.995,6.895;19.555,7.755"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="11,6.9"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="8.55"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="0.85"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O23">
      <dia:attribute name="obj_pos">
        <dia:point val="2.35,7.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.345,7.395;2.355,8.455"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.35,7.4"/>
        <dia:point val="2.35,8.45"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O0" connection="1"/>
        <dia:connection handle="1" to="O0" connection="6"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O24">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4,9.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.395,9.295;10.405,27.555"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.4,9.3"/>
        <dia:point val="10.4,27.55"/>
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
        <dia:point val="10.525,9.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.52,9.295;10.53,27.555"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.525,9.3"/>
        <dia:point val="10.525,27.55"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O1" connection="1"/>
        <dia:connection handle="1" to="O1" connection="6"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O26">
      <dia:attribute name="obj_pos">
        <dia:point val="19.5,9.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,9.845;19.505,9.855"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,9.85"/>
        <dia:point val="1.55,9.85"/>
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
        <dia:point val="19.5,10.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,10.695;19.505,10.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,10.7"/>
        <dia:point val="1.55,10.7"/>
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
        <dia:point val="19.5,11.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,11.545;19.505,11.555"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,11.55"/>
        <dia:point val="1.55,11.55"/>
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
        <dia:point val="19.5,12.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,12.345;19.505,12.355"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,12.35"/>
        <dia:point val="1.55,12.35"/>
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
        <dia:point val="19.5,13.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,13.195;19.505,13.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,13.2"/>
        <dia:point val="1.55,13.2"/>
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
        <dia:point val="19.5,14.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,14.045;19.505,14.055"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,14.05"/>
        <dia:point val="1.55,14.05"/>
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
        <dia:point val="19.5,14.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,14.895;19.505,14.905"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,14.9"/>
        <dia:point val="1.55,14.9"/>
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
        <dia:point val="19.5,15.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,15.745;19.505,15.755"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,15.75"/>
        <dia:point val="1.55,15.75"/>
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
        <dia:point val="19.5,16.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,16.545;19.505,16.555"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,16.55"/>
        <dia:point val="1.55,16.55"/>
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
        <dia:point val="19.5,17.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,17.395;19.505,17.405"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,17.4"/>
        <dia:point val="1.55,17.4"/>
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
        <dia:point val="19.5,18.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,18.245;19.505,18.255"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,18.25"/>
        <dia:point val="1.55,18.25"/>
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
        <dia:point val="19.5,19.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,19.095;19.505,19.105"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,19.1"/>
        <dia:point val="1.55,19.1"/>
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
        <dia:point val="19.5,19.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,19.945;19.505,19.955"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,19.95"/>
        <dia:point val="1.55,19.95"/>
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
        <dia:point val="19.5,20.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,20.745;19.505,20.755"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,20.75"/>
        <dia:point val="1.55,20.75"/>
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
        <dia:point val="19.5,21.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,21.595;19.505,21.605"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,21.6"/>
        <dia:point val="1.55,21.6"/>
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
        <dia:point val="19.5,22.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,22.445;19.505,22.455"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,22.45"/>
        <dia:point val="1.55,22.45"/>
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
        <dia:point val="19.5,23.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.52784,23.295;19.505,23.3061"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,23.3"/>
        <dia:point val="1.53284,23.3011"/>
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
        <dia:point val="19.5,24.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,24.145;19.505,24.155"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,24.15"/>
        <dia:point val="1.55,24.15"/>
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
        <dia:point val="19.5,25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,24.995;19.505,25.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,25"/>
        <dia:point val="1.55,25"/>
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
        <dia:point val="19.5,25.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,25.795;19.505,25.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,25.8"/>
        <dia:point val="1.55,25.8"/>
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
        <dia:point val="19.5,26.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,26.645;19.505,26.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,26.65"/>
        <dia:point val="1.55,26.65"/>
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
        <dia:point val="2.65,9.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.645,9.295;2.655,26.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.65,9.3"/>
        <dia:point val="2.65,26.65"/>
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
        <dia:point val="4.8,9.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.795,9.295;4.805,26.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.8,9.3"/>
        <dia:point val="4.8,26.65"/>
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
        <dia:point val="8,9.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.995,9.295;8.005,26.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,9.3"/>
        <dia:point val="8,26.65"/>
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
        <dia:point val="11.6,9.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.595,9.295;11.605,26.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.6,9.3"/>
        <dia:point val="11.6,26.65"/>
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
        <dia:point val="13.9,9.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.895,9.295;13.905,26.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9,9.3"/>
        <dia:point val="13.9,26.65"/>
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
        <dia:point val="17.1,9.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.095,9.295;17.105,27.555"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.1,9.3"/>
        <dia:point val="17.1,27.55"/>
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
        <dia:point val="1.55,26.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,26.745;10.405,27.555"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.55,26.75"/>
        <dia:point val="10.4,27.55"/>
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
        <dia:point val="19.5,26.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.545,26.745;19.505,26.755"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,26.75"/>
        <dia:point val="1.55,26.75"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O55">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,5.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.75,5.04825;4.578,5.44825"/>
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
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,5.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O56">
      <dia:attribute name="obj_pos">
        <dia:point val="1.65,7.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.65,6.96333;4.7204,7.34333"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#旧総合病院診療科#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.475000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.65,7.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.234118"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1919"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
  </dia:layer>
  <dia:layer name="文字" visible="true">
    <dia:object type="Standard - Text" version="0" id="O57">
      <dia:attribute name="obj_pos">
        <dia:point val="5.05861,1.98769"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.05861,1.53506;16.3969,2.13506"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#福 祉 医 療 費 給 付 事 業 総 括 表#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.05861,1.98769"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.36966"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.303"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O58">
      <dia:attribute name="obj_pos">
        <dia:point val="5.89334,3.33932"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.89334,2.96213;7.40834,3.46213"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#診療分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.89334,3.33932"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O59">
      <dia:attribute name="obj_pos">
        <dia:point val="11.2031,3.5891"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.2031,3.28735;14.4351,3.68735"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#医療機関等所在地#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.2031,3.5891"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O60">
      <dia:attribute name="obj_pos">
        <dia:point val="11.1855,5.12706"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.1855,4.82531;14.0135,5.22531"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#医療機関等名称#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.1855,5.12706"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O61">
      <dia:attribute name="obj_pos">
        <dia:point val="11.1148,6.71805"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.1148,6.4163;14.7508,6.8163"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#医療機関等電話番号#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.1148,6.71805"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O62">
      <dia:attribute name="obj_pos">
        <dia:point val="1.74332,9.68594"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.74332,9.4219;2.45032,9.7719"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#項番#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.74332,9.68594"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.215635"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.17675"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O63">
      <dia:attribute name="obj_pos">
        <dia:point val="2.71155,9.70558"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.71155,9.44154;4.83255,9.79154"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#市町村コード#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.71155,9.70558"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.215635"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.17675"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O64">
      <dia:attribute name="obj_pos">
        <dia:point val="5.83171,9.70172"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.83171,9.43768;7.24571,9.78768"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#市町村名#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.83171,9.70172"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.215635"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.17675"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O65">
      <dia:attribute name="obj_pos">
        <dia:point val="8.88045,9.69869"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.88045,9.43465;9.58745,9.78465"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.88045,9.69869"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.215635"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.17675"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O66">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7628,9.70039"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7628,9.43635;11.4698,9.78635"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#項番#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7628,9.70039"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.215635"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.17675"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O67">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7737,9.72735"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.7737,9.46331;13.8947,9.81331"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#市町村コード#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7737,9.72735"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.215635"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.17675"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O68">
      <dia:attribute name="obj_pos">
        <dia:point val="14.8585,9.68813"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.8585,9.42409;16.2725,9.77409"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#市町村名#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.8585,9.68813"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.215635"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.17675"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O69">
      <dia:attribute name="obj_pos">
        <dia:point val="17.9426,9.70278"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.9426,9.43874;18.6496,9.78874"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.9426,9.70278"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.215635"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.17675"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O70">
      <dia:attribute name="obj_pos">
        <dia:point val="1.61385,28.0461"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.61385,27.8198;7.43751,28.1198"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#注) 市町村コード順に記載してください。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.61385,28.0461"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O71">
      <dia:attribute name="obj_pos">
        <dia:point val="12.6604,27.2682"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.6604,26.9664;14.7006,27.3664"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#(合   計)#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.6604,27.2682"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O72">
      <dia:attribute name="obj_pos">
        <dia:point val="2.27226,10.4042"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86826,10.1024;2.27226,10.5024"/>
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
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.27226,10.4042"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O73">
      <dia:attribute name="obj_pos">
        <dia:point val="2.29951,11.3202"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.89551,11.0184;2.29951,11.4184"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.29951,11.3202"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O74">
      <dia:attribute name="obj_pos">
        <dia:point val="2.31887,12.1383"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.91487,11.8365;2.31887,12.2365"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#３#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.31887,12.1383"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O75">
      <dia:attribute name="obj_pos">
        <dia:point val="2.31887,12.9635"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.91487,12.6617;2.31887,13.0617"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#４#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.31887,12.9635"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O76">
      <dia:attribute name="obj_pos">
        <dia:point val="2.31387,13.8109"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.90987,13.5091;2.31387,13.9091"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#５#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.31387,13.8109"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O77">
      <dia:attribute name="obj_pos">
        <dia:point val="2.30451,14.6887"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.90051,14.3869;2.30451,14.7869"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#６#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.30451,14.6887"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O78">
      <dia:attribute name="obj_pos">
        <dia:point val="2.30451,15.5138"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.90051,15.212;2.30451,15.612"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#７#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.30451,15.5138"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O79">
      <dia:attribute name="obj_pos">
        <dia:point val="2.31291,16.339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.90891,16.0372;2.31291,16.4372"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#８#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.31291,16.339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O80">
      <dia:attribute name="obj_pos">
        <dia:point val="2.30951,17.173"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.90551,16.8712;2.30951,17.2712"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.30951,17.173"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O81">
      <dia:attribute name="obj_pos">
        <dia:point val="2.48454,18.0481"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.67654,17.7463;2.48454,18.1463"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１０#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.48454,18.0481"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O82">
      <dia:attribute name="obj_pos">
        <dia:point val="2.48454,18.8733"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.67654,18.5715;2.48454,18.9715"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１１#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.48454,18.8733"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O83">
      <dia:attribute name="obj_pos">
        <dia:point val="2.47954,19.6957"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.67154,19.3939;2.47954,19.7939"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１２#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.47954,19.6957"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O84">
      <dia:attribute name="obj_pos">
        <dia:point val="2.47018,20.5735"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.66218,20.2717;2.47018,20.6717"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１３#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.47018,20.5735"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O85">
      <dia:attribute name="obj_pos">
        <dia:point val="2.47018,21.3486"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.66218,21.0468;2.47018,21.4468"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１４#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.47018,21.3486"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O86">
      <dia:attribute name="obj_pos">
        <dia:point val="2.48794,22.2399"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.67994,21.9381;2.48794,22.3381"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１５#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.48794,22.2399"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O87">
      <dia:attribute name="obj_pos">
        <dia:point val="2.48454,23.0739"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.67654,22.7721;2.48454,23.1721"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１６#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.48454,23.0739"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O88">
      <dia:attribute name="obj_pos">
        <dia:point val="2.48454,23.924"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.67654,23.6222;2.48454,24.0222"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１７#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.48454,23.924"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O89">
      <dia:attribute name="obj_pos">
        <dia:point val="2.48454,24.7491"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.67654,24.4473;2.48454,24.8473"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１８#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.48454,24.7491"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O90">
      <dia:attribute name="obj_pos">
        <dia:point val="2.45453,25.5966"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.64653,25.2948;2.45453,25.6948"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.45453,25.5966"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O91">
      <dia:attribute name="obj_pos">
        <dia:point val="2.44518,26.4494"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.63718,26.1476;2.44518,26.5476"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２０#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.44518,26.4494"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O92">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4873,10.4226"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6793,10.1208;11.4873,10.5208"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２１#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4873,10.4226"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O93">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4873,11.2477"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6793,10.9459;11.4873,11.3459"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２２#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4873,11.2477"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O94">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4823,12.0702"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6743,11.7684;11.4823,12.1684"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２３#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4823,12.0702"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O95">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4729,12.948"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6649,12.6462;11.4729,13.0462"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２４#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4729,12.948"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O96">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4729,13.7231"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6649,13.4213;11.4729,13.8213"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２５#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4729,13.7231"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O97">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4907,14.6144"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6827,14.3126;11.4907,14.7126"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２６#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4907,14.6144"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O98">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4873,15.4484"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6793,15.1466;11.4873,15.5466"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２７#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4873,15.4484"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O99">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4873,16.2985"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6793,15.9967;11.4873,16.3967"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２８#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4873,16.2985"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O100">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4873,17.1236"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6793,16.8218;11.4873,17.2218"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4873,17.1236"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O101">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4573,17.9711"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6493,17.6693;11.4573,18.0693"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#３０#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4573,17.9711"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O102">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4979,18.8239"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6899,18.5221;11.4979,18.9221"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#３１#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4979,18.8239"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O103">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4623,19.6379"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6543,19.3361;11.4623,19.7361"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#３２#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4623,19.6379"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O104">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4623,20.463"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6543,20.1612;11.4623,20.5612"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#３３#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4623,20.463"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O105">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4573,21.2855"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6493,20.9837;11.4573,21.3837"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#３４#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4573,21.2855"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O106">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4479,22.1633"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6399,21.8615;11.4479,22.2615"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#３５#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4479,22.1633"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O107">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4479,22.9384"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6399,22.6366;11.4479,23.0366"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#３６#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4479,22.9384"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O108">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4657,23.8296"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6577,23.5278;11.4657,23.9278"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#３７#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4657,23.8296"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O109">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4623,24.6637"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6543,24.3619;11.4623,24.7619"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#３８#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4623,24.6637"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O110">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4623,25.5138"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6543,25.212;11.4623,25.612"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#３９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4623,25.5138"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O111">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4623,26.3389"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6543,26.0371;11.4623,26.4371"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#４０#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4623,26.3389"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
  </dia:layer>
  <dia:layer name="差し込み" visible="true">
    <dia:object type="Embed - Text" version="0" id="O112">
      <dia:attribute name="obj_pos">
        <dia:point val="1.50954,6.52666"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.50954,5.92315;9.58954,6.72315"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#HOSPCD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１２３４５６７８９０#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="1.000000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.50954,6.52666"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.49288"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.404"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HOSPCD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="10"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O113">
      <dia:attribute name="obj_pos">
        <dia:point val="11.0926,4.15099"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.0926,3.90204;19.4251,4.56204"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ADRS#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="50"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああああああああああああああああああああああ
ああああああああああああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.412500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.0926,4.15099"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.203313"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.16665"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#ADRS#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="100"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="25"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O114">
      <dia:attribute name="obj_pos">
        <dia:point val="11.1029,5.68944"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.1029,5.44049;17.7689,6.10049"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#HOSPNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="50"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああああああああああああ
ああああああああああああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.412500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.1029,5.68944"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.203313"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.16665"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HOSPNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="100"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="25"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O115">
      <dia:attribute name="obj_pos">
        <dia:point val="1.60646,3.37167"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.60646,2.99448;5.64646,3.49448"/>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.60646,3.37167"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
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
    <dia:object type="Embed - Text" version="0" id="O116">
      <dia:attribute name="obj_pos">
        <dia:point val="11.1,7.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.1,7.14825;14.7966,7.54825"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#TEL#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#123456789012345#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.1,7.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.24644"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.202"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TEL#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="15"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O117">
      <dia:attribute name="obj_pos">
        <dia:point val="1.6,8.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.6,7.64649;3.216,8.44649"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SRYKA#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１２#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="1.000000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.6,8.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.49288"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.404"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SRYKA#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="4"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O118">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,10.3839"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,10.1576;7.93247,10.4576"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[0].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,10.3839"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[0].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O119">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,11.2339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,11.0076;7.93247,11.3076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[1].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,11.2339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[1].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O120">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,12.0339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,11.8076;7.93247,12.1076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[2].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,12.0339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[2].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O121">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,12.8839"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,12.6576;7.93247,12.9576"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[3].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,12.8839"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[3].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O122">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,13.7339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,13.5076;7.93247,13.8076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[4].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,13.7339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[4].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O123">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,14.5839"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,14.3576;7.93247,14.6576"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[5].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,14.5839"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[5].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O124">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,15.4339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,15.2076;7.93247,15.5076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[6].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,15.4339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[6].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O125">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,16.2339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,16.0076;7.93247,16.3076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[7].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,16.2339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[7].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O126">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,17.0839"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,16.8576;7.93247,17.1576"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[8].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,17.0839"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[8].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O127">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,17.9339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,17.7076;7.93247,18.0076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[9].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,17.9339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[9].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O128">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,18.7839"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,18.5576;7.93247,18.8576"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[10].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,18.7839"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[10].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O129">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,19.6339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,19.4076;7.93247,19.7076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[11].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,19.6339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[11].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O130">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,20.4339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,20.2076;7.93247,20.5076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[12].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,20.4339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[12].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O131">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,21.2839"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,21.0576;7.93247,21.3576"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[13].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,21.2839"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[13].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O132">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,22.1339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,21.9076;7.93247,22.2076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[14].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,22.1339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[14].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O133">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,22.9839"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,22.7576;7.93247,23.0576"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[15].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,22.9839"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[15].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O134">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,23.8339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,23.6076;7.93247,23.9076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[16].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,23.8339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[16].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O135">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,24.6839"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,24.4576;7.93247,24.7576"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[17].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,24.6839"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[17].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O136">
      <dia:attribute name="obj_pos">
        <dia:point val="4.90247,25.4839"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.90247,25.2576;7.93247,25.5576"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[18].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.90247,25.4839"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[18].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O137">
      <dia:attribute name="obj_pos">
        <dia:point val="4.95247,26.3339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.95247,26.1076;7.98247,26.4076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[19].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.95247,26.3339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[19].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O138">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0025,10.3839"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0025,10.1576;17.0325,10.4576"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[20].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0025,10.3839"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[20].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O139">
      <dia:attribute name="obj_pos">
        <dia:point val="14,11.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14,11.0237;17.03,11.3237"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[21].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14,11.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[21].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O140">
      <dia:attribute name="obj_pos">
        <dia:point val="14,12.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14,11.8237;17.03,12.1237"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[22].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14,12.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[22].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O141">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0025,12.8839"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0025,12.6576;17.0325,12.9576"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[23].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0025,12.8839"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[23].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O142">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0025,13.7339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0025,13.5076;17.0325,13.8076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[24].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0025,13.7339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[24].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O143">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0025,14.5839"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0025,14.3576;17.0325,14.6576"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[25].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0025,14.5839"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[25].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O144">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0025,15.4339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0025,15.2076;17.0325,15.5076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[26].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0025,15.4339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[26].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O145">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0025,16.2839"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0025,16.0576;17.0325,16.3576"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[27].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0025,16.2839"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[27].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O146">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0025,17.0839"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0025,16.8576;17.0325,17.1576"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[28].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0025,17.0839"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[28].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O147">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0025,17.9339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0025,17.7076;17.0325,18.0076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[29].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0025,17.9339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[29].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O148">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0025,18.8339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0025,18.6076;17.0325,18.9076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[30].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0025,18.8339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[30].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O149">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0025,19.6339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0025,19.4076;17.0325,19.7076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[31].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0025,19.6339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[31].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O150">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0025,20.4839"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0025,20.2576;17.0325,20.5576"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[32].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0025,20.4839"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[32].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O151">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0025,21.3339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0025,21.1076;17.0325,21.4076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[33].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0025,21.3339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[33].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O152">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0025,22.1339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0025,21.9076;17.0325,22.2076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[34].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0025,22.1339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[34].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O153">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0025,22.9839"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0025,22.7576;17.0325,23.0576"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[35].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0025,22.9839"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[35].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O154">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0025,23.8339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0025,23.6076;17.0325,23.9076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[36].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0025,23.8339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[36].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O155">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0025,24.6839"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0025,24.4576;17.0325,24.7576"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[37].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0025,24.6839"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[37].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O156">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0025,25.5339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0025,25.3076;17.0325,25.6076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[38].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0025,25.5339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[38].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O157">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0025,26.3339"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0025,26.1076;17.0325,26.4076"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[39].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0025,26.3339"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.18483"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[39].CITYNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O158">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,10.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,10.0605;4.4135,10.5105"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[0].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,10.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[0].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O159">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,11.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,10.9105;4.4135,11.3605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[1].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,11.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[1].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O160">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,12.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,11.7105;4.4135,12.1605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[2].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,12.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[2].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O161">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,12.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,12.5605;4.4135,13.0105"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[3].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,12.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[3].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O162">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,13.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,13.4105;4.4135,13.8605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[4].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,13.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[4].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O163">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,14.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,14.2605;4.4135,14.7105"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[5].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,14.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[5].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O164">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,15.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,15.1105;4.4135,15.5605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[6].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,15.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[6].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O165">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,16.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,15.9105;4.4135,16.3605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[7].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,16.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[7].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O166">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,17.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,16.7605;4.4135,17.2105"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[8].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,17.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[8].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O167">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,17.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,17.6105;4.4135,18.0605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[9].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,17.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[9].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O168">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,18.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,18.4605;4.4135,18.9105"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[10].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,18.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[10].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O169">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,19.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,19.3105;4.4135,19.7605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[11].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,19.65"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[11].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O170">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,20.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,20.1105;4.4135,20.5605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[12].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,20.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[12].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O171">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,21.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,20.9605;4.4135,21.4105"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[13].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,21.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[13].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O172">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,22.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,21.8105;4.4135,22.2605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[14].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,22.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[14].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O173">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,23"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,22.6605;4.4135,23.1105"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[15].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,23"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[15].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O174">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,23.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,23.5105;4.4135,23.9605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[16].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,23.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[16].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O175">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,24.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,24.3605;4.4135,24.8105"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[17].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,24.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[17].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O176">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,25.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05,25.1605;4.4135,25.6105"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[18].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.05,25.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[18].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O177">
      <dia:attribute name="obj_pos">
        <dia:point val="3.1,26.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.1,26.0105;4.4635,26.4605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[19].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.1,26.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[19].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O178">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,10.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,10.0728;9.95,10.5728"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[0].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,10.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O179">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,11.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,10.9228;9.95,11.4228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[1].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,11.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O180">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,12.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,11.7228;9.95,12.2228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[2].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,12.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[2].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O181">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,12.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,12.5728;9.95,13.0728"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[3].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,12.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[3].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O182">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,13.4228;9.95,13.9228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[4].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,13.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[4].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O183">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,14.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,14.2728;9.95,14.7728"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[5].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,14.65"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[5].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O184">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,15.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,15.1228;9.95,15.6228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[6].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,15.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[6].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O185">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,16.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,15.9228;9.95,16.4228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[7].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,16.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[7].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O186">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,17.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,16.7728;9.95,17.2728"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[8].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,17.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[8].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O187">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,18"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,17.6228;9.95,18.1228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[9].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,18"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[9].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O188">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,18.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,18.4728;9.95,18.9728"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[10].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,18.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[10].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O189">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,19.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,19.3228;9.95,19.8228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[11].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,19.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[11].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O190">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,20.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,20.1228;9.95,20.6228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[12].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,20.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[12].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O191">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,21.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,20.9728;9.95,21.4728"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[13].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,21.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[13].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O192">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,22.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,21.8228;9.95,22.3228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[14].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,22.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[14].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O193">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,23.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,22.6728;9.95,23.1728"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[15].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,23.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[15].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O194">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,23.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,23.5228;9.95,24.0228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[16].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,23.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[16].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O195">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,24.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,24.3728;9.95,24.8728"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[17].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,24.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[17].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O196">
      <dia:attribute name="obj_pos">
        <dia:point val="9.95,25.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.40975,25.1728;9.95,25.6728"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[18].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.95,25.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[18].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O197">
      <dia:attribute name="obj_pos">
        <dia:point val="10,26.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.45975,26.0228;10,26.5228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[19].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10,26.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[19].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O198">
      <dia:attribute name="obj_pos">
        <dia:point val="12.15,10.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.15,10.0605;13.5135,10.5105"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[20].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.15,10.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[20].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O199">
      <dia:attribute name="obj_pos">
        <dia:point val="12.1475,11.2661"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.1475,10.9266;13.511,11.3766"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[21].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.1475,11.2661"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[21].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O200">
      <dia:attribute name="obj_pos">
        <dia:point val="12.1475,12.0661"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.1475,11.7266;13.511,12.1766"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[22].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.1475,12.0661"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[22].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O201">
      <dia:attribute name="obj_pos">
        <dia:point val="12.15,12.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.15,12.5605;13.5135,13.0105"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[23].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.15,12.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[23].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O202">
      <dia:attribute name="obj_pos">
        <dia:point val="12.15,13.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.15,13.4105;13.5135,13.8605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[24].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.15,13.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[24].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O203">
      <dia:attribute name="obj_pos">
        <dia:point val="12.15,14.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.15,14.2605;13.5135,14.7105"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[25].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.15,14.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[25].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O204">
      <dia:attribute name="obj_pos">
        <dia:point val="12.15,15.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.15,15.1105;13.5135,15.5605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[26].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.15,15.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[26].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O205">
      <dia:attribute name="obj_pos">
        <dia:point val="12.15,16.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.15,15.9605;13.5135,16.4105"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[27].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.15,16.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[27].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O206">
      <dia:attribute name="obj_pos">
        <dia:point val="12.15,17.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.15,16.7605;13.5135,17.2105"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[28].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.15,17.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[28].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O207">
      <dia:attribute name="obj_pos">
        <dia:point val="12.15,17.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.15,17.6105;13.5135,18.0605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[29].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.15,17.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[29].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O208">
      <dia:attribute name="obj_pos">
        <dia:point val="12.15,18.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.15,18.5105;13.5135,18.9605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[30].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.15,18.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[30].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O209">
      <dia:attribute name="obj_pos">
        <dia:point val="12.15,19.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.15,19.3105;13.5135,19.7605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[31].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.15,19.65"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[31].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O210">
      <dia:attribute name="obj_pos">
        <dia:point val="12.15,20.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.15,20.1605;13.5135,20.6105"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[32].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.15,20.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[32].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O211">
      <dia:attribute name="obj_pos">
        <dia:point val="12.15,21.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.15,21.0105;13.5135,21.4605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[33].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.15,21.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[33].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O212">
      <dia:attribute name="obj_pos">
        <dia:point val="12.15,22.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.15,21.8105;13.5135,22.2605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[34].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.15,22.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[34].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O213">
      <dia:attribute name="obj_pos">
        <dia:point val="12.15,23"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.15,22.6605;13.5135,23.1105"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[35].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.15,23"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[35].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O214">
      <dia:attribute name="obj_pos">
        <dia:point val="12.15,23.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.15,23.5105;13.5135,23.9605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[36].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.15,23.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[36].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O215">
      <dia:attribute name="obj_pos">
        <dia:point val="12.15,24.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.15,24.3605;13.5135,24.8105"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[37].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.15,24.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[37].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O216">
      <dia:attribute name="obj_pos">
        <dia:point val="12.15,25.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.15,25.2105;13.5135,25.6605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[38].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.15,25.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[38].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O217">
      <dia:attribute name="obj_pos">
        <dia:point val="12.15,26.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.15,26.0105;13.5135,26.4605"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[39].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.15,26.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.277245"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[39].CITYNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O218">
      <dia:attribute name="obj_pos">
        <dia:point val="19.1,27.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.2517,26.9728;19.1,27.4728"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#GKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.1,27.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#GKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O219">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,10.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5098,10.0728;19.05,10.5728"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[20].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,10.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[20].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O220">
      <dia:attribute name="obj_pos">
        <dia:point val="19.0475,11.3161"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5072,10.9389;19.0475,11.4389"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[21].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.0475,11.3161"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[21].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O221">
      <dia:attribute name="obj_pos">
        <dia:point val="19.0475,12.1161"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5072,11.7389;19.0475,12.2389"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[22].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.0475,12.1161"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[22].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O222">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,12.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5098,12.5728;19.05,13.0728"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[23].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,12.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[23].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O223">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5098,13.4228;19.05,13.9228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[24].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,13.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[24].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O224">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,14.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5098,14.2728;19.05,14.7728"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[25].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,14.65"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[25].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O225">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,15.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5098,15.1228;19.05,15.6228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[26].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,15.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[26].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O226">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,16.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5098,15.9728;19.05,16.4728"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[27].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,16.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[27].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O227">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,17.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5098,16.7728;19.05,17.2728"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[28].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,17.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[28].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O228">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,18"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5098,17.6228;19.05,18.1228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[29].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,18"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[29].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O229">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,18.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5098,18.5228;19.05,19.0228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[30].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,18.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[30].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O230">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,19.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5098,19.3228;19.05,19.8228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[31].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,19.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[31].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O231">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,20.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5098,20.1728;19.05,20.6728"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[32].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,20.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[32].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O232">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,21.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5098,21.0228;19.05,21.5228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[33].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,21.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[33].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O233">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,22.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5098,21.8228;19.05,22.3228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[34].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,22.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[34].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O234">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,23.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5098,22.6728;19.05,23.1728"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[35].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,23.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[35].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O235">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,23.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5098,23.5228;19.05,24.0228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[36].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,23.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[36].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O236">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,24.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5098,24.3728;19.05,24.8728"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[37].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,24.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[37].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O237">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,25.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5098,25.2228;19.05,25.7228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[38].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,25.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[38].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O238">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,26.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5098,26.0228;19.05,26.5228"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU[39].KENSU#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,26.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.30805"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU[39].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
  </dia:layer>
</dia:diagram>
