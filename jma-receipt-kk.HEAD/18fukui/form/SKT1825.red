<?xml version="1.0" encoding="UTF-8"?>
<dia:diagram xmlns:dia="http://www.lysator.liu.se/~alla/dia/">
  <dia:dictionarydata>
    <dia:element name="SRYYM" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="16"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="HKNJANUM" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="16"/>
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
    <dia:element name="HOSPCD" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="14"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="HOSPNAME" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="100"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="NO" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="4"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="SEIKYU-TBL" occurs="2">
      <dia:element name="NYUGAI-TBL" occurs="2">
        <dia:element name="KENSU" occurs="1">
          <dia:appinfo>
            <dia:embed object="string" length="5"/>
          </dia:appinfo>
        </dia:element>
        <dia:element name="NISSU" occurs="1">
          <dia:appinfo>
            <dia:embed object="string" length="5"/>
          </dia:appinfo>
        </dia:element>
        <dia:element name="TENSU" occurs="1">
          <dia:appinfo>
            <dia:embed object="string" length="9"/>
          </dia:appinfo>
        </dia:element>
        <dia:element name="ITBFTN" occurs="1">
          <dia:appinfo>
            <dia:embed object="string" length="9"/>
          </dia:appinfo>
        </dia:element>
      </dia:element>
      <dia:element name="S-KENSU" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="5"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="S-NISSU" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="5"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="S-KINGK" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="9"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="S-FTNGK" occurs="1">
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
  <dia:layer name="A4縦枠" visible="false">
    <dia:group>
      <dia:object type="Standard - Box" version="0" id="O0">
        <dia:attribute name="obj_pos">
          <dia:point val="0,0"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="-0.025,-0.025;21.01,29.75"/>
        </dia:attribute>
        <dia:attribute name="elem_corner">
          <dia:point val="0,0"/>
        </dia:attribute>
        <dia:attribute name="elem_width">
          <dia:real val="20.985"/>
        </dia:attribute>
        <dia:attribute name="elem_height">
          <dia:real val="29.725"/>
        </dia:attribute>
        <dia:attribute name="border_width">
          <dia:real val="0.05"/>
        </dia:attribute>
        <dia:attribute name="border_color">
          <dia:color val="#fd211d"/>
        </dia:attribute>
        <dia:attribute name="show_background">
          <dia:boolean val="false"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Box" version="0" id="O1">
        <dia:attribute name="obj_pos">
          <dia:point val="0.82403,0.874181"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.79903,0.849181;19.7569,28.8549"/>
        </dia:attribute>
        <dia:attribute name="elem_corner">
          <dia:point val="0.82403,0.874181"/>
        </dia:attribute>
        <dia:attribute name="elem_width">
          <dia:real val="18.9079"/>
        </dia:attribute>
        <dia:attribute name="elem_height">
          <dia:real val="27.9557"/>
        </dia:attribute>
        <dia:attribute name="border_width">
          <dia:real val="0.05"/>
        </dia:attribute>
        <dia:attribute name="border_color">
          <dia:color val="#fb1e3f"/>
        </dia:attribute>
        <dia:attribute name="show_background">
          <dia:boolean val="false"/>
        </dia:attribute>
      </dia:object>
    </dia:group>
  </dia:layer>
  <dia:layer name="線" visible="true">
    <dia:object type="Standard - Box" version="0" id="O2">
      <dia:attribute name="obj_pos">
        <dia:point val="1.7,13.4181"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.69,13.4081;19.11,26.515"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="1.7,13.4181"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="17.4"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="13.0869"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O3">
      <dia:attribute name="obj_pos">
        <dia:point val="1.7,6.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.69,6.29;1.71,12.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.7,6.3"/>
        <dia:point val="1.7,12.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O4">
      <dia:attribute name="obj_pos">
        <dia:point val="1.7,6.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.69,6.29;19.26,6.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.7,6.3"/>
        <dia:point val="19.25,6.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O5">
      <dia:attribute name="obj_pos">
        <dia:point val="4.5,6.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.49,6.84;19.26,6.86"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.5,6.85"/>
        <dia:point val="19.25,6.85"/>
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
        <dia:point val="1.7,7.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.69,7.49;19.26,7.51"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.7,7.5"/>
        <dia:point val="19.25,7.5"/>
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
        <dia:point val="2.6,8.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.59,8.69;19.26,8.71"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.6,8.7"/>
        <dia:point val="19.25,8.7"/>
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
        <dia:point val="1.7,9.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.69,9.89;19.26,9.91"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.7,9.9"/>
        <dia:point val="19.25,9.9"/>
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
        <dia:point val="2.6,11.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.59,11.09;19.26,11.11"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.6,11.1"/>
        <dia:point val="19.25,11.1"/>
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
        <dia:point val="1.7,12.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.69,12.29;19.26,12.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.7,12.3"/>
        <dia:point val="19.25,12.3"/>
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
        <dia:point val="2.6,7.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.59,7.49;2.61,12.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.6,7.5"/>
        <dia:point val="2.6,12.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O12">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,7.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.04,7.49;3.06,12.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.05,7.5"/>
        <dia:point val="3.05,12.3"/>
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
        <dia:point val="4.5,6.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.49,6.29;4.51,12.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.5,6.3"/>
        <dia:point val="4.5,12.3"/>
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
        <dia:point val="5.9,6.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.89,6.84;5.91,12.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,6.85"/>
        <dia:point val="5.9,12.3"/>
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
        <dia:point val="7.3,6.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.29,6.84;7.31,12.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="7.3,6.85"/>
        <dia:point val="7.3,12.3"/>
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
        <dia:point val="9.85,6.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.84,6.84;9.86,12.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="9.85,6.85"/>
        <dia:point val="9.85,12.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O17">
      <dia:attribute name="obj_pos">
        <dia:point val="12.4,6.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.39,6.29;12.41,12.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.4,6.3"/>
        <dia:point val="12.4,12.3"/>
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
        <dia:point val="13.8,6.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.79,6.84;13.81,12.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.8,6.85"/>
        <dia:point val="13.8,12.3"/>
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
        <dia:point val="15.2,6.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.19,6.84;15.21,12.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.2,6.85"/>
        <dia:point val="15.2,12.3"/>
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
        <dia:point val="17.15,6.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.14,6.84;17.16,12.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.15,6.85"/>
        <dia:point val="17.15,12.3"/>
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
        <dia:point val="19.25,6.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.24,6.29;19.26,12.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.25,6.3"/>
        <dia:point val="19.25,12.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O22">
      <dia:attribute name="obj_pos">
        <dia:point val="1.2,27.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.19,27.09;19.51,27.11"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.2,27.1"/>
        <dia:point val="19.5,27.1"/>
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
        <dia:point val="1.2,27.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.19,1.44;1.21,27.11"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.2,27.1"/>
        <dia:point val="1.2,1.45"/>
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
        <dia:point val="1.2,1.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.19,1.44;19.51,1.46"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.2,1.45"/>
        <dia:point val="19.5,1.45"/>
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
        <dia:point val="19.5,27.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.49,1.44;19.51,27.11"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.5,27.1"/>
        <dia:point val="19.5,1.45"/>
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
        <dia:point val="11.7,3.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.69,3.54;18.46,3.56"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.7,3.55"/>
        <dia:point val="18.45,3.55"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O27">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,8.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.04,8.09;19.26,8.11"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.05,8.1"/>
        <dia:point val="19.25,8.1"/>
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
        <dia:point val="3.05,9.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.04,9.29;19.26,9.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.05,9.3"/>
        <dia:point val="19.25,9.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="1" to="O21" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O29">
      <dia:attribute name="obj_pos">
        <dia:point val="3.05,10.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.04,10.49;19.26,10.51"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.05,10.5"/>
        <dia:point val="19.25,10.5"/>
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
        <dia:point val="3.05,11.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.04,11.69;19.26,11.71"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.05,11.7"/>
        <dia:point val="19.25,11.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O31">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,8.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.39,8.09;13.81,8.71"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.8,8.1"/>
        <dia:point val="12.4,8.7"/>
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
        <dia:point val="15.2,8.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.79,8.09;15.21,8.71"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.2,8.1"/>
        <dia:point val="13.8,8.7"/>
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
        <dia:point val="17.15,8.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.19,8.09;17.16,8.71"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.15,8.1"/>
        <dia:point val="15.2,8.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O34">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,8.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.14,8.09;19.26,8.71"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.25,8.1"/>
        <dia:point val="17.15,8.7"/>
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
        <dia:point val="13.8,9.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.39,9.29;13.81,9.91"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.8,9.3"/>
        <dia:point val="12.4,9.9"/>
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
        <dia:point val="15.2,9.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.79,9.29;15.21,9.91"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.2,9.3"/>
        <dia:point val="13.8,9.9"/>
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
        <dia:point val="17.15,9.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.19,9.29;17.16,9.91"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.15,9.3"/>
        <dia:point val="15.2,9.9"/>
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
        <dia:point val="19.25,9.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.14,9.29;19.26,9.91"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.25,9.3"/>
        <dia:point val="17.15,9.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O21" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O39">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,10.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.39,10.49;13.81,11.11"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.8,10.5"/>
        <dia:point val="12.4,11.1"/>
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
        <dia:point val="15.2,10.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.79,10.49;15.21,11.11"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.2,10.5"/>
        <dia:point val="13.8,11.1"/>
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
        <dia:point val="17.15,10.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.19,10.49;17.16,11.11"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.15,10.5"/>
        <dia:point val="15.2,11.1"/>
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
        <dia:point val="19.25,10.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.14,10.49;19.26,11.11"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.25,10.5"/>
        <dia:point val="17.15,11.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O43">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,11.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.39,11.69;13.81,12.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.8,11.7"/>
        <dia:point val="12.4,12.3"/>
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
        <dia:point val="15.2,11.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.79,11.69;15.21,12.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.2,11.7"/>
        <dia:point val="13.8,12.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O45">
      <dia:attribute name="obj_pos">
        <dia:point val="17.15,11.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.19,11.69;17.16,12.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.15,11.7"/>
        <dia:point val="15.2,12.3"/>
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
        <dia:point val="19.25,11.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.14,11.69;19.26,12.31"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.25,11.7"/>
        <dia:point val="17.15,12.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O47">
      <dia:attribute name="obj_pos">
        <dia:point val="1.68878,3.50857"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.68378,3.50357;6.51074,4.36163"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="1.68878,3.50857"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="4.81696"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="0.848057"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O48">
      <dia:attribute name="obj_pos">
        <dia:point val="2.30716,3.50857"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.30216,3.50357;2.31216,4.37929"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.30716,3.50857"/>
        <dia:point val="2.30716,4.37429"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O49">
      <dia:attribute name="obj_pos">
        <dia:point val="2.90226,3.5012"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.89726,3.4962;2.90726,4.37193"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.90226,3.5012"/>
        <dia:point val="2.90226,4.36693"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O50">
      <dia:attribute name="obj_pos">
        <dia:point val="3.47323,3.50857"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.46823,3.50357;3.48574,4.35434"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.47323,3.50857"/>
        <dia:point val="3.48074,4.34934"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O51">
      <dia:attribute name="obj_pos">
        <dia:point val="4.04507,3.48353"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.04007,3.47853;4.05007,4.35426"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.04507,3.48353"/>
        <dia:point val="4.04507,4.34926"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O52">
      <dia:attribute name="obj_pos">
        <dia:point val="4.65138,3.50857"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.64638,3.50357;4.65638,4.37929"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.65138,3.50857"/>
        <dia:point val="4.65138,4.37429"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O53">
      <dia:attribute name="obj_pos">
        <dia:point val="5.27148,3.5012"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.26648,3.4962;5.27648,4.37193"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.27148,3.5012"/>
        <dia:point val="5.27148,4.36693"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O54">
      <dia:attribute name="obj_pos">
        <dia:point val="5.91012,3.5012"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.90512,3.4962;5.91512,4.37193"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.91012,3.5012"/>
        <dia:point val="5.91012,4.36693"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
  </dia:layer>
  <dia:layer name="文字" visible="true">
    <dia:object type="Standard - Text" version="0" id="O55">
      <dia:attribute name="obj_pos">
        <dia:point val="9.125,2.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.125,1.74167;16.0334,2.19167"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#診療報酬請求内訳書(医科・歯科)#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.125,2.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2727"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O56">
      <dia:attribute name="obj_pos">
        <dia:point val="5.2,7.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8061,7.06111;5.5939,7.36111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件 数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.2,7.3"/>
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
    <dia:object type="Standard - Text" version="0" id="O57">
      <dia:attribute name="obj_pos">
        <dia:point val="13.1,7.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.7061,7.06111;13.4939,7.36111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件 数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.1,7.3"/>
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
    <dia:object type="Standard - Text" version="0" id="O58">
      <dia:attribute name="obj_pos">
        <dia:point val="8.55,7.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.0046,7.06111;9.0954,7.36111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点 　数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.55,7.3"/>
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
    <dia:object type="Standard - Text" version="0" id="O59">
      <dia:attribute name="obj_pos">
        <dia:point val="16.15,7.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5743,7.06111;16.7257,7.36111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#金   額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.15,7.3"/>
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
    <dia:object type="Standard - Text" version="0" id="O60">
      <dia:attribute name="obj_pos">
        <dia:point val="8.3,6.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.99205,6.4213;9.60795,6.7713"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#療 養 の 給 付#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.3,6.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2121"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.17675"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O61">
      <dia:attribute name="obj_pos">
        <dia:point val="11.1,7.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.3425,7.06111;11.8575,7.36111"/>
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
            <dia:point val="11.1,7.3"/>
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
    <dia:object type="Standard - Text" version="0" id="O62">
      <dia:attribute name="obj_pos">
        <dia:point val="18.25,7.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.4925,7.06111;19.0075,7.36111"/>
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
            <dia:point val="18.25,7.3"/>
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
    <dia:object type="Standard - Text" version="0" id="O63">
      <dia:attribute name="obj_pos">
        <dia:point val="14.5,7.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.1061,7.06111;14.8939,7.36111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#回 数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.5,7.3"/>
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
    <dia:object type="Standard - Text" version="0" id="O64">
      <dia:attribute name="obj_pos">
        <dia:point val="15.8,6.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.2093,6.4213;17.3908,6.7713"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.8,6.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2121"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.17675"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O65">
      <dia:attribute name="obj_pos">
        <dia:point val="9.35371,4.73436"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.35371,4.45566;11.8282,5.15566"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#保険医療機関の
名称#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.35371,4.73436"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2121"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.17675"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O66">
      <dia:attribute name="obj_pos">
        <dia:point val="12.9,3.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.6628,3.1713;14.1373,3.5213"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.9,3.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2121"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.17675"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O67">
      <dia:attribute name="obj_pos">
        <dia:point val="12.35,8.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.1682,7.90667;12.35,8.08667"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#円#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.225000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.35,8.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.10908"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.0909"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O68">
      <dia:attribute name="obj_pos">
        <dia:point val="19.2,8.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.0182,7.90667;19.2,8.08667"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#円#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.225000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.2,8.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.10908"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.0909"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O69">
      <dia:attribute name="obj_pos">
        <dia:point val="17.1,8.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.9182,7.90667;17.1,8.08667"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#円#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.225000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.1,8.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.10908"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.0909"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O70">
      <dia:attribute name="obj_pos">
        <dia:point val="1.85,6.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.85,5.96111;3.971,6.26111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#後期高齢者医療#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.85,6.2"/>
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
    <dia:object type="Standard - Text" version="0" id="O71">
      <dia:attribute name="obj_pos">
        <dia:point val="1.87668,26.3286"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87668,26.0897;7.08828,26.3897"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#注意  ※印の欄には記入しないこと。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.87668,26.3286"/>
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
    <dia:object type="Standard - Text" version="0" id="O72">
      <dia:attribute name="obj_pos">
        <dia:point val="8.31935,2.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.31935,1.74167;8.77385,2.19167"/>
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
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.31935,2.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2727"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O73">
      <dia:attribute name="obj_pos">
        <dia:point val="3.75,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.4773,7.685;4.0227,7.955"/>
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
            <dia:real val="0.337500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.75,7.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.16362"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.13635"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O74">
      <dia:attribute name="obj_pos">
        <dia:point val="3.75,8.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.34095,8.285;4.15905,8.555"/>
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
            <dia:real val="0.337500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.75,8.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.16362"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.13635"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O75">
      <dia:attribute name="obj_pos">
        <dia:point val="2.85,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.71365,7.685;2.98635,8.495"/>
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
            <dia:real val="0.337500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.85,7.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.16362"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.13635"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O76">
      <dia:attribute name="obj_pos">
        <dia:point val="3.75,9.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.4773,8.885;4.0227,9.155"/>
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
            <dia:real val="0.337500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.75,9.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.16362"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.13635"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O77">
      <dia:attribute name="obj_pos">
        <dia:point val="3.75,9.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.34095,9.485;4.15905,9.755"/>
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
            <dia:real val="0.337500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.75,9.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.16362"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.13635"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O78">
      <dia:attribute name="obj_pos">
        <dia:point val="2.85,10.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.71365,10.085;2.98635,10.895"/>
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
            <dia:real val="0.337500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.85,10.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.16362"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.13635"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O79">
      <dia:attribute name="obj_pos">
        <dia:point val="1.65,3.35901"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.65,3.08031;3.4175,3.43031"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.65,3.35901"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2121"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.17675"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O80">
        <dia:attribute name="obj_pos">
          <dia:point val="2.05,11.6"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.05,11.377;2.3328,11.937"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#七
割#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.05,11.6"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
            <dia:attribute name="single">
              <dia:real val="0.16968"/>
            </dia:attribute>
            <dia:attribute name="multi">
              <dia:real val="0.1414"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="0" id="O81">
        <dia:attribute name="obj_pos">
          <dia:point val="2.05,10.5"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.05,10.277;2.3328,11.397"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#後
期
高
齢#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.05,10.5"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
            <dia:attribute name="single">
              <dia:real val="0.16968"/>
            </dia:attribute>
            <dia:attribute name="multi">
              <dia:real val="0.1414"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
      </dia:object>
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O82">
        <dia:attribute name="obj_pos">
          <dia:point val="2.05,9.15"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.05,8.92704;2.3328,9.48704"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#九
割#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.05,9.15"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
            <dia:attribute name="single">
              <dia:real val="0.16968"/>
            </dia:attribute>
            <dia:attribute name="multi">
              <dia:real val="0.1414"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="0" id="O83">
        <dia:attribute name="obj_pos">
          <dia:point val="2.05,8.05"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.05,7.82704;2.3328,8.94704"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#後
期
高
齢#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.05,8.05"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
            <dia:attribute name="single">
              <dia:real val="0.16968"/>
            </dia:attribute>
            <dia:attribute name="multi">
              <dia:real val="0.1414"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
      </dia:object>
    </dia:group>
    <dia:object type="Standard - Text" version="0" id="O84">
      <dia:attribute name="obj_pos">
        <dia:point val="2.85,9.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.71365,8.885;2.98635,9.695"/>
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
            <dia:real val="0.337500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.85,9.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.16362"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.13635"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O85">
      <dia:attribute name="obj_pos">
        <dia:point val="2.85,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.71365,11.285;2.98635,12.095"/>
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
            <dia:real val="0.337500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.85,11.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.16362"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.13635"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O86">
      <dia:attribute name="obj_pos">
        <dia:point val="3.75,10.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.4773,10.085;4.0227,10.355"/>
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
            <dia:real val="0.337500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.75,10.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.16362"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.13635"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O87">
      <dia:attribute name="obj_pos">
        <dia:point val="3.75,10.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.34095,10.685;4.15905,10.955"/>
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
            <dia:real val="0.337500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.75,10.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.16362"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.13635"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O88">
      <dia:attribute name="obj_pos">
        <dia:point val="3.75,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.4773,11.285;4.0227,11.555"/>
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
            <dia:real val="0.337500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.75,11.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.16362"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.13635"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O89">
      <dia:attribute name="obj_pos">
        <dia:point val="3.75,12.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.34095,11.885;4.15905,12.155"/>
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
            <dia:real val="0.337500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.75,12.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.16362"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.13635"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O90">
      <dia:attribute name="obj_pos">
        <dia:point val="6.6,7.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.9637,6.91111;7.2363,7.51111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#診 療
実 日 数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6,7.15"/>
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
    <dia:object type="Standard - Text" version="0" id="O91">
      <dia:attribute name="obj_pos">
        <dia:point val="1.90786,13.8649"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.90786,13.626;2.87746,13.926"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#備  考#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.90786,13.8649"/>
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
  </dia:layer>
  <dia:layer name="差し込み" visible="true">
    <dia:object type="Embed - Text" version="0" id="O92">
      <dia:attribute name="obj_pos">
        <dia:point val="11.877,4.725"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.877,4.48611;19.452,5.08611"/>
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
            <dia:string>#あああああああああああああああああああああああああ
あああああああああああああああああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.877,4.725"/>
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
        <dia:string>#HOSPNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="100"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="25"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O93">
      <dia:attribute name="obj_pos">
        <dia:point val="14.75,3.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.75,3.10185;18.285,3.60185"/>
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
            <dia:string>#９９９９９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.75,3.5"/>
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
        <dia:string>#HOSPCD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O94">
      <dia:attribute name="obj_pos">
        <dia:point val="4.65,2.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.65,1.74167;8.286,2.19167"/>
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
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.65,2.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.2727"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
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
    <dia:object type="Embed - Text" version="0" id="O95">
      <dia:attribute name="obj_pos">
        <dia:point val="7.12647,5.59364"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.12647,5.11586;7.73247,5.71586"/>
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
            <dia:point val="7.12647,5.59364"/>
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
    <dia:object type="Embed - Text" version="0" id="O96">
      <dia:attribute name="obj_pos">
        <dia:point val="7,5.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7,4.99352;7.9595,5.94352"/>
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
            <dia:string>#◯#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="1.187500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7,5.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.5757"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.47975"/>
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
    <dia:object type="Embed - Text" version="0" id="O97">
      <dia:attribute name="obj_pos">
        <dia:point val="19.05,1.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.3228,1.56111;19.05,1.86111"/>
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
            <dia:string>#1234#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.05,1.8"/>
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
    <dia:object type="Embed - Text" version="0" id="O98">
      <dia:attribute name="obj_pos">
        <dia:point val="16.9,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2638,7.66111;16.9,7.96111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[0].S-KINGK#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.9,7.9"/>
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
        <dia:string>#SEIKYU-TBL[0].S-KINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O99">
      <dia:attribute name="obj_pos">
        <dia:point val="19,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3638,7.66111;19,7.96111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[0].S-FTNGK#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19,7.9"/>
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
        <dia:string>#SEIKYU-TBL[0].S-FTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O100">
      <dia:attribute name="obj_pos">
        <dia:point val="16.9,10.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2638,10.0611;16.9,10.3611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[1].S-KINGK#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.9,10.3"/>
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
        <dia:string>#SEIKYU-TBL[1].S-KINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O101">
      <dia:attribute name="obj_pos">
        <dia:point val="18.95,10.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.3138,10.0611;18.95,10.3611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[1].S-FTNGK#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.95,10.3"/>
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
        <dia:string>#SEIKYU-TBL[1].S-FTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O102">
      <dia:attribute name="obj_pos">
        <dia:point val="5.7,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.791,7.66111;5.7,7.96111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[0].KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.7,7.9"/>
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
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O103">
      <dia:attribute name="obj_pos">
        <dia:point val="7.1,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.191,7.66111;7.1,7.96111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[0].NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.1,7.9"/>
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
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[0].NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O104">
      <dia:attribute name="obj_pos">
        <dia:point val="9.6,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.9638,7.66111;9.6,7.96111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[0].TENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.6,7.9"/>
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
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[0].TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O105">
      <dia:attribute name="obj_pos">
        <dia:point val="12.075,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.4388,7.66111;12.075,7.96111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[0].ITBFTN#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.075,7.9"/>
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
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O106">
      <dia:attribute name="obj_pos">
        <dia:point val="13.65,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.741,7.66111;13.65,7.96111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[0].S-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.65,7.9"/>
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
        <dia:string>#SEIKYU-TBL[0].S-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O107">
      <dia:attribute name="obj_pos">
        <dia:point val="15.05,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.141,7.66111;15.05,7.96111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[0].S-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.05,7.9"/>
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
        <dia:string>#SEIKYU-TBL[0].S-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O108">
      <dia:attribute name="obj_pos">
        <dia:point val="5.7,8.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.791,8.31111;5.7,8.61111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[1].KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.7,8.55"/>
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
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O109">
      <dia:attribute name="obj_pos">
        <dia:point val="7.1,8.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.191,8.31111;7.1,8.61111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[1].NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.1,8.55"/>
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
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[1].NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O110">
      <dia:attribute name="obj_pos">
        <dia:point val="9.6,8.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.9638,8.31111;9.6,8.61111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[1].TENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.6,8.55"/>
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
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[1].TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O111">
      <dia:attribute name="obj_pos">
        <dia:point val="12.075,8.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.4388,8.31111;12.075,8.61111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[1].ITBFTN#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.075,8.55"/>
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
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O112">
      <dia:attribute name="obj_pos">
        <dia:point val="5.7,10.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.791,10.0611;5.7,10.3611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[0].KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.7,10.3"/>
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
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O113">
      <dia:attribute name="obj_pos">
        <dia:point val="7.075,10.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.166,10.0611;7.075,10.3611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[0].NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.075,10.3"/>
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
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[0].NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O114">
      <dia:attribute name="obj_pos">
        <dia:point val="9.6,10.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.9638,10.0611;9.6,10.3611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[0].TENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.6,10.3"/>
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
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[0].TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O115">
      <dia:attribute name="obj_pos">
        <dia:point val="12.05,10.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.4138,10.0611;12.05,10.3611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[0].ITBFTN#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.05,10.3"/>
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
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O116">
      <dia:attribute name="obj_pos">
        <dia:point val="13.65,10.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.741,10.0611;13.65,10.3611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[1].S-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.65,10.3"/>
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
        <dia:string>#SEIKYU-TBL[1].S-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O117">
      <dia:attribute name="obj_pos">
        <dia:point val="15.05,10.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.141,10.0611;15.05,10.3611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[1].S-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.05,10.3"/>
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
        <dia:string>#SEIKYU-TBL[1].S-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O118">
      <dia:attribute name="obj_pos">
        <dia:point val="5.7,10.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.791,10.6611;5.7,10.9611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[1].KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.7,10.9"/>
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
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O119">
      <dia:attribute name="obj_pos">
        <dia:point val="7.075,10.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.166,10.6611;7.075,10.9611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[1].NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.075,10.9"/>
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
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[1].NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O120">
      <dia:attribute name="obj_pos">
        <dia:point val="9.575,10.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.9388,10.6611;9.575,10.9611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[1].TENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.575,10.9"/>
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
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[1].TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O121">
      <dia:attribute name="obj_pos">
        <dia:point val="12.05,10.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.4138,10.6611;12.05,10.9611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[1].ITBFTN#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.05,10.9"/>
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
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O122">
      <dia:attribute name="obj_pos">
        <dia:point val="1.70945,4.16228"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.70945,3.6845;6.55745,4.2845"/>
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
            <dia:string>#１２３４５６７８#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.70945,4.16228"/>
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
        <dia:string>#HKNJANUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="16"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
  </dia:layer>
</dia:diagram>
