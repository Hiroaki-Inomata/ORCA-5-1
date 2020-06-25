<?xml version="1.0" encoding="UTF-8"?>
<dia:diagram xmlns:dia="http://www.lysator.liu.se/~alla/dia/">
  <dia:dictionarydata>
    <element name="SEIYMD" occurs="1">
      <appinfo>
        <embed object="text" length="22"/>
      </appinfo>
    </element>
    <element name="HOSPCD" occurs="1">
      <appinfo>
        <embed object="text" length="7"/>
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
    <element name="ZAI1" occurs="1">
      <appinfo>
        <embed object="text" length="2"/>
      </appinfo>
    </element>
    <element name="ZAI2" occurs="1">
      <appinfo>
        <embed object="text" length="2"/>
      </appinfo>
    </element>
    <element name="TOKUBETU" occurs="1">
      <appinfo>
        <embed object="text" length="10"/>
      </appinfo>
    </element>
    <element name="HKN-TBL" occurs="2">
      <element name="MAISU" occurs="1">
        <appinfo>
          <embed object="text" length="5"/>
        </appinfo>
      </element>
      <element name="NYUGAI-TBL" occurs="2">
        <element name="KENSU" occurs="1">
          <appinfo>
            <embed object="text" length="5"/>
          </appinfo>
        </element>
        <element name="TENSU" occurs="1">
          <appinfo>
            <embed object="text" length="14"/>
          </appinfo>
        </element>
      </element>
    </element>
    <element name="ETC-TBL" occurs="2">
      <element name="ETC-KENSU" occurs="1">
        <appinfo>
          <embed object="text" length="5"/>
        </appinfo>
      </element>
      <element name="ETC-TENSU" occurs="1">
        <appinfo>
          <embed object="text" length="14"/>
        </appinfo>
      </element>
    </element>
    <element name="FUKUSI-TBL" occurs="2">
      <element name="F-KENSU" occurs="4">
        <appinfo>
          <embed object="text" length="5"/>
        </appinfo>
      </element>
      <element name="F-TENSU" occurs="4">
        <appinfo>
          <embed object="text" length="11"/>
        </appinfo>
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
  <dia:layer name="背景" visible="true">
    <dia:object type="Standard - Box" version="0" id="O0">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,2.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.78,2.08;19.02,28.22"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="1.8,2.1"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="17.199999999999999"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="26.100000000000001"/>
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
        <dia:point val="1.8,7.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.78,7.78;19.02,7.82"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.8,7.8"/>
        <dia:point val="19,7.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O2">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,9.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.78,9.08;19.02,9.12"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.8,9.1"/>
        <dia:point val="19,9.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O3">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,12.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.78,12.08;19.02,12.12"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.8,12.1"/>
        <dia:point val="19,12.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O4">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,13.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.78,13.38;19.02,13.42"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.8,13.4"/>
        <dia:point val="19,13.4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O5">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,16.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.78,16.28;19.02,16.32"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.8,16.3"/>
        <dia:point val="19,16.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O6">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,16.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.78,16.28;19.02,16.32"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.8,16.3"/>
        <dia:point val="19,16.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O7">
      <dia:attribute name="obj_pos">
        <dia:point val="10.8,16.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7751,16.28;10.82,25.0263"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.8,16.3"/>
        <dia:point val="10.7951,25.0063"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O8">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.78,24.98;19.02,25.02"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.8,25"/>
        <dia:point val="19,25"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O9">
      <dia:attribute name="obj_pos">
        <dia:point val="13,4.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.98,4.48;18.62,4.52"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13,4.5"/>
        <dia:point val="18.6,4.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O10">
      <dia:attribute name="obj_pos">
        <dia:point val="10.9,6.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.88,6.88;18.72,6.92"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.9,6.9"/>
        <dia:point val="18.7,6.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O11">
      <dia:attribute name="obj_pos">
        <dia:point val="8,10.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.995,10.095;16.405,10.105"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,10.1"/>
        <dia:point val="16.4,10.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O12">
      <dia:attribute name="obj_pos">
        <dia:point val="8,11.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.995,11.095;16.405,11.105"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,11.1"/>
        <dia:point val="16.4,11.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O13">
      <dia:attribute name="obj_pos">
        <dia:point val="8,8.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.995,8.495;16.405,8.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,8.5"/>
        <dia:point val="16.4,8.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O14">
      <dia:attribute name="obj_pos">
        <dia:point val="16.4,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.395,7.895;16.405,16.305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="16.4,7.9"/>
        <dia:point val="16.4,16.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O15">
      <dia:attribute name="obj_pos">
        <dia:point val="8,7.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.995,7.795;8.005,16.305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,7.8"/>
        <dia:point val="8,16.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O16">
      <dia:attribute name="obj_pos">
        <dia:point val="8,14.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.995,14.295;16.405,14.305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,14.3"/>
        <dia:point val="16.4,14.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O17">
      <dia:attribute name="obj_pos">
        <dia:point val="8,15.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.995,15.295;16.405,15.305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,15.3"/>
        <dia:point val="16.4,15.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O18">
      <dia:attribute name="obj_pos">
        <dia:point val="10,8.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.995,8.495;10.005,12.105"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10,8.5"/>
        <dia:point val="10,12.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O19">
      <dia:attribute name="obj_pos">
        <dia:point val="12.2,8.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.195,8.495;12.205,12.105"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.2,8.5"/>
        <dia:point val="12.2,12.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O20">
      <dia:attribute name="obj_pos">
        <dia:point val="10,12.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.995,12.695;10.005,16.305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10,12.7"/>
        <dia:point val="10,16.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O21">
      <dia:attribute name="obj_pos">
        <dia:point val="12.2,12.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.195,12.695;12.205,16.305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.2,12.7"/>
        <dia:point val="12.2,16.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O22">
      <dia:attribute name="obj_pos">
        <dia:point val="8,12.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.995,12.695;16.405,12.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,12.7"/>
        <dia:point val="16.4,12.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O23">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,17.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.78,17.18;19.02,17.22"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.8,17.2"/>
        <dia:point val="19,17.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O24">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,21.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.78,21.18;10.82,21.22"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.8,21.2"/>
        <dia:point val="10.8,21.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O25">
      <dia:attribute name="obj_pos">
        <dia:point val="4.9,7.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.88,7.78;4.92,25.02"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.9,7.8"/>
        <dia:point val="4.9,25"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O26">
      <dia:attribute name="obj_pos">
        <dia:point val="3.5,17.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.48,17.18;3.52,25.02"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.5,17.2"/>
        <dia:point val="3.5,25"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O27">
      <dia:attribute name="obj_pos">
        <dia:point val="2.6,17.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.58,17.18;2.62,25.02"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.6,17.2"/>
        <dia:point val="2.6,25"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.040000000000000001"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O28">
      <dia:attribute name="obj_pos">
        <dia:point val="2.6,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.595,18.195;10.805,18.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.6,18.2"/>
        <dia:point val="10.8,18.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O29">
      <dia:attribute name="obj_pos">
        <dia:point val="2.6,19.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.595,19.195;10.805,19.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.6,19.2"/>
        <dia:point val="10.8,19.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O30">
      <dia:attribute name="obj_pos">
        <dia:point val="2.6,20.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.595,20.195;10.805,20.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.6,20.2"/>
        <dia:point val="10.8,20.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O31">
      <dia:attribute name="obj_pos">
        <dia:point val="2.6,22.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.595,22.095;10.805,22.105"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.6,22.1"/>
        <dia:point val="10.8,22.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O32">
      <dia:attribute name="obj_pos">
        <dia:point val="2.6,23.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.595,23.095;10.805,23.105"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.6,23.1"/>
        <dia:point val="10.8,23.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O33">
      <dia:attribute name="obj_pos">
        <dia:point val="3.5,24.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.495,24.095;10.805,24.105"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.5,24.1"/>
        <dia:point val="10.8,24.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O34">
      <dia:attribute name="obj_pos">
        <dia:point val="6.3,16.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.295,16.295;6.305,25.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6.3,16.3"/>
        <dia:point val="6.3,25"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O35">
      <dia:attribute name="obj_pos">
        <dia:point val="9.1,16.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.095,16.295;9.105,25.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="9.1,16.3"/>
        <dia:point val="9.1,25"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O36">
      <dia:attribute name="obj_pos">
        <dia:point val="10.8,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.795,17.895;19.005,17.905"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.8,17.9"/>
        <dia:point val="19,17.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O37">
      <dia:attribute name="obj_pos">
        <dia:point val="10.8,18.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.795,18.495;19.005,18.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.8,18.5"/>
        <dia:point val="19,18.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O38">
      <dia:attribute name="obj_pos">
        <dia:point val="10.8,19.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.795,19.195;19.005,19.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.8,19.2"/>
        <dia:point val="19,19.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O39">
      <dia:attribute name="obj_pos">
        <dia:point val="10.8,19.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.795,19.795;19.005,19.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.8,19.8"/>
        <dia:point val="19,19.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O40">
      <dia:attribute name="obj_pos">
        <dia:point val="10.8,20.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.795,20.495;19.005,20.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.8,20.5"/>
        <dia:point val="19,20.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O41">
      <dia:attribute name="obj_pos">
        <dia:point val="10.8,21.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.795,21.095;19.005,21.105"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.8,21.1"/>
        <dia:point val="19,21.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.5"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O42">
      <dia:attribute name="obj_pos">
        <dia:point val="10.8,21.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.795,21.795;19.005,21.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.8,21.8"/>
        <dia:point val="19,21.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.5"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O43">
      <dia:attribute name="obj_pos">
        <dia:point val="10.8,22.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.795,22.395;19.005,22.405"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.8,22.4"/>
        <dia:point val="19,22.4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.5"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O44">
      <dia:attribute name="obj_pos">
        <dia:point val="10.8,23.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.795,23.095;19.005,23.105"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.8,23.1"/>
        <dia:point val="19,23.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.5"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O45">
      <dia:attribute name="obj_pos">
        <dia:point val="10.8,23.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.795,23.695;19.005,23.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.8,23.7"/>
        <dia:point val="19,23.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.5"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O46">
      <dia:attribute name="obj_pos">
        <dia:point val="10.8,24.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.795,24.395;19.005,24.405"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.8,24.4"/>
        <dia:point val="19,24.4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.5"/>
      </dia:attribute>
    </dia:object>
  </dia:layer>
  <dia:layer name="文字" visible="true" active="true">
    <dia:object type="Standard - Text" version="1" id="O47">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,2.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,2.53;12.18,2.9525"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#国民健康保険・後期高齢者医療診療報酬等請求総括票#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52500000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.1,2.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O48">
      <dia:attribute name="obj_pos">
        <dia:point val="7,4.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7,4.335;8.2,4.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#審査提出#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7,4.6"/>
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
    <dia:object type="Standard - Text" version="1" id="O49">
      <dia:attribute name="obj_pos">
        <dia:point val="2.3,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.3,4.935;2.6,5.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#北#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.3,5.2"/>
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
    <dia:object type="Standard - Text" version="1" id="O50">
      <dia:attribute name="obj_pos">
        <dia:point val="2.7,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.7,4.935;3,5.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#海#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.7,5.2"/>
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
    <dia:object type="Standard - Text" version="1" id="O51">
      <dia:attribute name="obj_pos">
        <dia:point val="3.1,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.1,4.935;3.4,5.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#道#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.1,5.2"/>
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
    <dia:object type="Standard - Text" version="1" id="O52">
      <dia:attribute name="obj_pos">
        <dia:point val="3.5,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.5,4.935;3.8,5.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#国#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.5,5.2"/>
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
    <dia:object type="Standard - Text" version="1" id="O53">
      <dia:attribute name="obj_pos">
        <dia:point val="3.9,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.9,4.935;4.2,5.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#民#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.9,5.2"/>
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
    <dia:object type="Standard - Text" version="1" id="O54">
      <dia:attribute name="obj_pos">
        <dia:point val="4.3,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.3,4.935;4.6,5.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#健#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.3,5.2"/>
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
    <dia:object type="Standard - Text" version="1" id="O55">
      <dia:attribute name="obj_pos">
        <dia:point val="4.7,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.7,4.935;5,5.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#康#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.7,5.2"/>
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
    <dia:object type="Standard - Text" version="1" id="O56">
      <dia:attribute name="obj_pos">
        <dia:point val="5,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5,4.935;5.3,5.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#保#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5,5.2"/>
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
    <dia:object type="Standard - Text" version="1" id="O57">
      <dia:attribute name="obj_pos">
        <dia:point val="5.4,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4,4.935;5.7,5.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#険#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.4,5.2"/>
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
    <dia:object type="Standard - Text" version="1" id="O58">
      <dia:attribute name="obj_pos">
        <dia:point val="5.8,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.8,4.935;6.1,5.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#団#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.8,5.2"/>
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
    <dia:object type="Standard - Text" version="1" id="O59">
      <dia:attribute name="obj_pos">
        <dia:point val="6.2,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2,4.935;6.5,5.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#体#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.2,5.2"/>
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
        <dia:point val="6.6,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.6,4.935;6.9,5.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#連#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.6,5.2"/>
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
        <dia:point val="7,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7,4.935;7.3,5.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#合#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7,5.2"/>
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
        <dia:point val="7.4,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.4,4.935;7.7,5.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#会#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.4,5.2"/>
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
        <dia:point val="7.8,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.8,4.935;8.1,5.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#御#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.8,5.2"/>
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
        <dia:point val="8.2,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.2,4.935;8.5,5.2375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#中#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.2,5.2"/>
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
        <dia:point val="2.4,5.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.4,5.635;8.7,5.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（札幌市中央区南２条西１４丁目国保会館内）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.4,5.9"/>
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
        <dia:point val="8.9,6.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.9,5.99;11,6.3425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#保険医療機関#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.9,6.3"/>
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
        <dia:point val="8.9,6.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.9,6.49;11,6.8425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#所在地・名称#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.9,6.8"/>
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
        <dia:point val="11.7,3.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.7,3.49;13.1,3.8425"/>
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
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.7,3.8"/>
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
        <dia:point val="11.7,4.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.7,4.035;12,4.3375"/>
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
            <dia:point val="11.7,4.3"/>
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
        <dia:point val="12.2,4.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2,4.035;12.5,4.3375"/>
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
            <dia:point val="12.2,4.3"/>
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
        <dia:point val="12.7,4.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.7,4.035;13,4.3375"/>
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
            <dia:point val="12.7,4.3"/>
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
        <dia:point val="18.3,6.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.3,6.4875;18.54,6.73"/>
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
            <dia:real val="0.29999999999999999"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.3,6.7"/>
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
        <dia:point val="18.2117,6.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.2117,6.3975;18.6117,6.8"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.2117,6.75"/>
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
        <dia:point val="10.6,8.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.6,8.035;10.9,8.3375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#療#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,8.3"/>
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
        <dia:point val="11.3,8.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.3,8.035;11.6,8.3375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#養#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.3,8.3"/>
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
        <dia:point val="12.1,8.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.1,8.035;12.4,8.3375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#の#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.1,8.3"/>
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
        <dia:point val="12.7,8.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.7,8.035;13,8.3375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#給#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.7,8.3"/>
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
        <dia:point val="13.5,8.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.5,8.035;13.8,8.3375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#付#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.5,8.3"/>
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
        <dia:point val="8.5,8.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5,8.635;8.8,8.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#区#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.5,8.9"/>
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
        <dia:point val="9.2,8.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.2,8.635;9.5,8.9375"/>
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
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.2,8.9"/>
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
        <dia:point val="10.4,8.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.4,8.635;10.7,8.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4,8.9"/>
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
        <dia:point val="11.5,8.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.5,8.635;11.8,8.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.5,8.9"/>
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
        <dia:point val="13.4,8.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.4,8.635;13.7,8.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4,8.9"/>
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
        <dia:point val="14.9,8.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.9,8.635;15.2,8.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.9,8.9"/>
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
        <dia:point val="17,8.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17,8.335;17.3,8.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#備#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17,8.6"/>
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
        <dia:point val="18.1,8.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.1,8.335;18.4,8.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#考#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.1,8.6"/>
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
        <dia:point val="17,12.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17,12.535;17.3,12.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#備#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17,12.8"/>
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
        <dia:point val="18.1,12.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.1,12.535;18.4,12.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#考#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.1,12.8"/>
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
        <dia:point val="10.5,12.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5,12.235;10.8,12.5375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#療#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.5,12.5"/>
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
        <dia:point val="11.2,12.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.2,12.235;11.5,12.5375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#養#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.2,12.5"/>
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
        <dia:point val="12,12.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12,12.235;12.3,12.5375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#の#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12,12.5"/>
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
        <dia:point val="12.6,12.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.6,12.235;12.9,12.5375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#給#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.6,12.5"/>
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
        <dia:point val="13.4,12.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.4,12.235;13.7,12.5375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#付#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4,12.5"/>
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
        <dia:point val="8.4,13.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.4,12.835;8.7,13.1375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#区#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.4,13.1"/>
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
        <dia:point val="9.1,13.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.1,12.835;9.4,13.1375"/>
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
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.1,13.1"/>
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
        <dia:point val="10.3,13.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.3,12.835;10.6,13.1375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,13.1"/>
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
        <dia:point val="11.4,13.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.4,12.835;11.7,13.1375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4,13.1"/>
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
        <dia:point val="13.3,13.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.3,12.835;13.6,13.1375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.3,13.1"/>
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
        <dia:point val="14.8,13.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.8,12.835;15.1,13.1375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.8,13.1"/>
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
        <dia:point val="8.5,9.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5,9.535;9.4,9.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入  院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.5,9.8"/>
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
        <dia:point val="8.5,10.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5,10.535;9.4,10.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.5,10.8"/>
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
        <dia:point val="8.5,11.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5,11.435;8.8,11.7375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#合#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.5,11.7"/>
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
        <dia:point val="9.1,11.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.1,11.435;9.4,11.7375"/>
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
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.1,11.7"/>
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
        <dia:point val="8.5,14"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5,13.735;9.4,14.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入  院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.5,14"/>
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
        <dia:point val="8.5,15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5,14.735;9.4,15.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入院外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.5,15"/>
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
        <dia:point val="8.5,15.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5,15.635;8.8,15.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#合#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.5,15.9"/>
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
        <dia:point val="9.1,15.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.1,15.635;9.4,15.9375"/>
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
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.1,15.9"/>
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
        <dia:point val="2.5,8.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5,8.335;4.15,8.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#区   分  １#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.5,8.6"/>
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
        <dia:point val="2.5,12.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5,12.535;4.15,12.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#区   分  ２#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.5,12.8"/>
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
        <dia:point val="2.3,10.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.3,10.3725;4.52,10.745"/>
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
            <dia:real val="0.46250000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.3,10.7"/>
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
        <dia:point val="2,14.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2,14.5725;4.59,14.945"/>
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
            <dia:real val="0.46250000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2,14.9"/>
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
        <dia:point val="2.5,12.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5,12.535;2.5,12.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>##</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.5,12.8"/>
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
        <dia:point val="5.4,8.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4,8.2825;7.56,8.645"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#請求書の枚数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.45000000000000001"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.4,8.6"/>
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
        <dia:point val="5.4,12.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.4,12.4825;7.56,12.845"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#請求書の枚数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.45000000000000001"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.4,12.8"/>
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
        <dia:point val="2.1,19.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,19.035;2.4,19.3375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#障#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.1,19.3"/>
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
        <dia:point val="2.1,22.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,21.835;2.4,22.1375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ひ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.1,22.1"/>
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
        <dia:point val="2.1,22.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,22.635;2.4,22.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#と#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.1,22.9"/>
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
        <dia:point val="2.1,23.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,23.335;2.4,23.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#り#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.1,23.6"/>
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
        <dia:point val="2.1,24.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,24.135;2.4,24.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#親#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.1,24.4"/>
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
        <dia:point val="2.9,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.9,17.5475;3.3,17.95"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#71#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.9,17.9"/>
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
        <dia:point val="2.9,18.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.9,18.4475;3.3,18.85"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#71#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.9,18.8"/>
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
        <dia:point val="2.9,19.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.9,19.4475;3.3,19.85"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#72#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.9,19.8"/>
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
        <dia:point val="2.9,20.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.9,20.4475;3.3,20.85"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#73#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.9,20.8"/>
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
        <dia:point val="2.9,21.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.9,21.4475;3.3,21.85"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#75#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.9,21.8"/>
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
        <dia:point val="2.9,22.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.9,22.4475;3.3,22.85"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#75#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.9,22.8"/>
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
        <dia:point val="2.9,24.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.9,23.8475;3.3,24.25"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#77#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.9,24.2"/>
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
        <dia:point val="2.7375,18.0125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.7375,17.395;3.4375,18.0975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.875"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.7375,18.0125"/>
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
        <dia:point val="2.7375,18.925"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.7375,18.3075;3.4375,19.01"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.875"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.7375,18.925"/>
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
        <dia:point val="2.7625,19.9125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.7625,19.295;3.4625,19.9975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.875"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.7625,19.9125"/>
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
        <dia:point val="2.75,20.9125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.75,20.295;3.45,20.9975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.875"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.75,20.9125"/>
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
        <dia:point val="2.7625,21.9125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.7625,21.295;3.4625,21.9975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.875"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.7625,21.9125"/>
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
        <dia:point val="2.7375,22.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.7375,22.2825;3.4375,22.985"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.875"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.7375,22.9"/>
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
        <dia:point val="2.75,24.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.75,23.6825;3.45,24.385"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.875"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.75,24.3"/>
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
        <dia:point val="4.1,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.1,17.635;4.4,17.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#割#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.1,17.9"/>
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
        <dia:point val="3.8,18.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.8,18.435;4.7,18.7375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#限度額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.8,18.7"/>
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
        <dia:point val="3.7,18.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.7,18.705;4.8,18.9275"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#(入院のみ)#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.27500000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.7,18.9"/>
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
        <dia:point val="3.8,19.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.8,19.335;4.1,19.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#前#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.8,19.6"/>
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
    <dia:object type="Standard - Text" version="1" id="O138">
      <dia:attribute name="obj_pos">
        <dia:point val="4.4,19.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.4,19.335;4.7,19.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#期#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.4,19.6"/>
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
    <dia:object type="Standard - Text" version="1" id="O139">
      <dia:attribute name="obj_pos">
        <dia:point val="3.8,20"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.8,19.735;4.7,20.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#高齢者#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.8,20"/>
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
    <dia:object type="Standard - Text" version="1" id="O140">
      <dia:attribute name="obj_pos">
        <dia:point val="3.8,20.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.8,20.535;4.1,20.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#障#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.8,20.8"/>
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
    <dia:object type="Standard - Text" version="1" id="O141">
      <dia:attribute name="obj_pos">
        <dia:point val="4.4,20.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.4,20.535;4.7,20.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#老#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.4,20.8"/>
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
    <dia:object type="Standard - Text" version="1" id="O142">
      <dia:attribute name="obj_pos">
        <dia:point val="4.1,21.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.1,21.535;4.4,21.8375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#割#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.1,21.8"/>
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
    <dia:object type="Standard - Text" version="1" id="O143">
      <dia:attribute name="obj_pos">
        <dia:point val="3.8,22.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.8,22.335;4.7,22.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#限度額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.8,22.6"/>
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
    <dia:object type="Standard - Text" version="1" id="O144">
      <dia:attribute name="obj_pos">
        <dia:point val="3.6,22.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.6,22.68;4.85,22.9325"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#(入院のみ)#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.6,22.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O145">
      <dia:attribute name="obj_pos">
        <dia:point val="3.7,23.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.7,23.4525;4.82,23.735"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#長期高額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.34999999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.7,23.7"/>
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
    <dia:object type="Standard - Text" version="1" id="O146">
      <dia:attribute name="obj_pos">
        <dia:point val="3.8,24.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.8,24.435;4.7,24.7375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#結・精#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.8,24.7"/>
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
    <dia:object type="Standard - Text" version="1" id="O147">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,25.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,25.29;2.45,25.6425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#1.#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.1,25.6"/>
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
    <dia:object type="Standard - Text" version="1" id="O148">
      <dia:attribute name="obj_pos">
        <dia:point val="2.7,25.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.7,25.3175;19.02,25.64"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#この用紙はその月の審査に提出される保険者ごとの請求書を集計したものですので、１ヵ月１枚添付して下さい。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.40000000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.7,25.6"/>
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
    <dia:object type="Standard - Text" version="1" id="O149">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,26"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,25.69;2.45,26.0425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#2.#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.1,26"/>
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
    <dia:object type="Standard - Text" version="1" id="O150">
      <dia:attribute name="obj_pos">
        <dia:point val="2.7,26"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.7,25.735;9.3,26.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#「区分１」の国民健康保険の件数・点数欄には、#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.7,26"/>
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
    <dia:object type="Standard - Text" version="1" id="O151">
      <dia:attribute name="obj_pos">
        <dia:point val="9.6,26"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.6,25.735;9.9,26.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#25#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.6,26"/>
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
    <dia:object type="Standard - Text" version="1" id="O152">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4875,26.0875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.4875,25.6475;9.9875,26.15"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4875,26.0875"/>
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
    <dia:object type="Standard - Text" version="1" id="O153">
      <dia:attribute name="obj_pos">
        <dia:point val="10.1,26"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.1,25.735;12.2,26.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#一般被保険者と#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.1,26"/>
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
    <dia:object type="Standard - Text" version="1" id="O154">
      <dia:attribute name="obj_pos">
        <dia:point val="12.3,26"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.3,25.735;12.6,26.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#67#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.3,26"/>
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
    <dia:object type="Standard - Text" version="1" id="O155">
      <dia:attribute name="obj_pos">
        <dia:point val="12.2,26.0875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2,25.6475;12.7,26.15"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.2,26.0875"/>
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
    <dia:object type="Standard - Text" version="1" id="O156">
      <dia:attribute name="obj_pos">
        <dia:point val="12.8,26"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.8,25.735;18.5,26.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#退職者を合算した数字を記載して下さい。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.8,26"/>
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
    <dia:object type="Standard - Text" version="1" id="O157">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,26.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,26.19;2.45,26.5425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#3.#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.1,26.5"/>
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
    <dia:object type="Standard - Text" version="1" id="O158">
      <dia:attribute name="obj_pos">
        <dia:point val="2.7,26.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.7,26.235;9.6,26.5375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#「区分２」の後期高齢者医療の件数・点数欄には、#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.7,26.5"/>
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
    <dia:object type="Standard - Text" version="1" id="O159">
      <dia:attribute name="obj_pos">
        <dia:point val="9.9,26.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.9,26.235;10.2,26.5375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#09#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.9,26.5"/>
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
    <dia:object type="Standard - Text" version="1" id="O160">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4,26.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.4,26.235;11.7,26.5375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#07#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4,26.5"/>
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
    <dia:object type="Standard - Text" version="1" id="O161">
      <dia:attribute name="obj_pos">
        <dia:point val="9.8,26.575"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.8,26.135;10.3,26.6375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.8,26.575"/>
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
    <dia:object type="Standard - Text" version="1" id="O162">
      <dia:attribute name="obj_pos">
        <dia:point val="11.3125,26.5875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.3125,26.1475;11.8125,26.65"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.3125,26.5875"/>
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
    <dia:object type="Standard - Text" version="1" id="O163">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,26.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.3,26.235;11.2,26.5375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９割と#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,26.5"/>
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
    <dia:object type="Standard - Text" version="1" id="O164">
      <dia:attribute name="obj_pos">
        <dia:point val="11.9,26.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.9,26.235;17.3,26.5375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#７割を合算した数字を記載して下さい。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.9,26.5"/>
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
    <dia:object type="Standard - Text" version="1" id="O165">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,27"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,26.69;2.45,27.0425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#4.#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.1,27"/>
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
    <dia:object type="Standard - Text" version="1" id="O166">
      <dia:attribute name="obj_pos">
        <dia:point val="2.7,27"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.7,26.735;18,27.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#「請求書の枚数」欄は、レセプト電算処理システム及びオンライン請求システムを導入している医療機関について#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.7,27"/>
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
    <dia:object type="Standard - Text" version="1" id="O167">
      <dia:attribute name="obj_pos">
        <dia:point val="2.7,27.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.7,27.135;7.2,27.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#は、記載する必要はありません。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.7,27.4"/>
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
    <dia:object type="Standard - Text" version="1" id="O168">
      <dia:attribute name="obj_pos">
        <dia:point val="7.3,27.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.3,27.135;14.2,27.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙レセプトで請求する場合のみ記載して下さい）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.3,27.4"/>
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
    <dia:object type="Standard - Text" version="1" id="O169">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,27.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,27.59;2.45,27.9425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#5.#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.1,27.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O170">
      <dia:attribute name="obj_pos">
        <dia:point val="2.8,27.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.8,27.635;7.3,27.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※印の欄は記入しないで下さい。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.8,27.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O171">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5,16.635;4.15,16.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#区   分  ３#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.5,16.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O172">
      <dia:attribute name="obj_pos">
        <dia:point val="5.1,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.1,16.635;5.4,16.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.1,16.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O173">
      <dia:attribute name="obj_pos">
        <dia:point val="5.8,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.8,16.635;6.1,16.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.8,16.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O174">
      <dia:attribute name="obj_pos">
        <dia:point val="6.7,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.7,16.635;7.3,16.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.7,16.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O175">
      <dia:attribute name="obj_pos">
        <dia:point val="7.5,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.5,16.635;8.7,16.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（金額）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.5,16.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O176">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.4,16.635;10.45,16.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#備   考#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,16.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O177">
      <dia:attribute name="obj_pos">
        <dia:point val="8.8,19.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8,18.88;9.05,19.1325"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#円#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.8,19.1"/>
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
    <dia:object type="Standard - Text" version="1" id="O178">
      <dia:attribute name="obj_pos">
        <dia:point val="8.8,20.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8,19.88;9.05,20.1325"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#円#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.8,20.1"/>
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
    <dia:object type="Standard - Text" version="1" id="O179">
      <dia:attribute name="obj_pos">
        <dia:point val="8.8,21"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8,20.78;9.05,21.0325"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#円#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.8,21"/>
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
    <dia:object type="Standard - Text" version="1" id="O180">
      <dia:attribute name="obj_pos">
        <dia:point val="8.8,23"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8,22.78;9.05,23.0325"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#円#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.8,23"/>
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
    <dia:object type="Standard - Text" version="1" id="O181">
      <dia:attribute name="obj_pos">
        <dia:point val="8.8,24"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8,23.78;9.05,24.0325"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#円#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.8,24"/>
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
    <dia:object type="Standard - Text" version="1" id="O182">
      <dia:attribute name="obj_pos">
        <dia:point val="8.8,24.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8,24.68;9.05,24.9325"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#円#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.3125"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.8,24.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O183">
      <dia:attribute name="obj_pos">
        <dia:point val="12.6,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.6,16.635;12.9,16.9375"/>
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
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.6,16.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O184">
      <dia:attribute name="obj_pos">
        <dia:point val="13.3,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.3,16.635;13.6,16.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#連#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.3,16.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O185">
      <dia:attribute name="obj_pos">
        <dia:point val="14,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14,16.635;14.3,16.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#合#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14,16.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O186">
      <dia:attribute name="obj_pos">
        <dia:point val="14.8,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.8,16.635;15.1,16.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#会#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.8,16.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O187">
      <dia:attribute name="obj_pos">
        <dia:point val="15.5,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5,16.635;15.8,16.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#使#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.5,16.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O188">
      <dia:attribute name="obj_pos">
        <dia:point val="16.2,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.2,16.635;16.5,16.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#用#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.2,16.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O189">
      <dia:attribute name="obj_pos">
        <dia:point val="16.9,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.9,16.635;17.2,16.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#欄#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.9,16.9"/>
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
    <dia:object type="Standard - Text" version="1" id="O190">
      <dia:attribute name="obj_pos">
        <dia:point val="11.3,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.3,17.435;11.6,17.7375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#受#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.3,17.7"/>
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
    <dia:object type="Standard - Text" version="1" id="O191">
      <dia:attribute name="obj_pos">
        <dia:point val="12,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12,17.435;12.3,17.7375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#付#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12,17.7"/>
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
    <dia:object type="Standard - Text" version="1" id="O192">
      <dia:attribute name="obj_pos">
        <dia:point val="12.7,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.7,17.435;13,17.7375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#月#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.7,17.7"/>
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
    <dia:object type="Standard - Text" version="1" id="O193">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.4,17.435;13.7,17.7375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#日#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4,17.7"/>
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
    <dia:object type="Standard - Text" version="1" id="O194">
      <dia:attribute name="obj_pos">
        <dia:point val="11.3,18.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.3,18.035;11.6,18.3375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#提#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.3,18.3"/>
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
    <dia:object type="Standard - Text" version="1" id="O195">
      <dia:attribute name="obj_pos">
        <dia:point val="12,18.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12,18.035;12.3,18.3375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#出#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12,18.3"/>
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
    <dia:object type="Standard - Text" version="1" id="O196">
      <dia:attribute name="obj_pos">
        <dia:point val="12.7,18.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.7,18.035;13,18.3375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#方#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.7,18.3"/>
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
    <dia:object type="Standard - Text" version="1" id="O197">
      <dia:attribute name="obj_pos">
        <dia:point val="13.3,18.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.3,18.035;13.6,18.3375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#法#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.3,18.3"/>
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
    <dia:object type="Standard - Text" version="1" id="O198">
      <dia:attribute name="obj_pos">
        <dia:point val="16.1,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.1,17.435;16.4,17.7375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#月#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.1,17.7"/>
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
    <dia:object type="Standard - Text" version="1" id="O199">
      <dia:attribute name="obj_pos">
        <dia:point val="17.4,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.4,17.435;17.7,17.7375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#日#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.4,17.7"/>
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
    <dia:object type="Standard - Text" version="1" id="O200">
      <dia:attribute name="obj_pos">
        <dia:point val="14.5,18.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.5,18.035;18.7,18.3375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#持参・郵送・媒体・オンライン#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.5,18.3"/>
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
    <dia:object type="Standard - Text" version="1" id="O201">
      <dia:attribute name="obj_pos">
        <dia:point val="11.3,19"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.3,18.735;11.6,19.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#受#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.3,19"/>
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
    <dia:object type="Standard - Text" version="1" id="O202">
      <dia:attribute name="obj_pos">
        <dia:point val="12,19"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12,18.735;12.3,19.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#付#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12,19"/>
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
    <dia:object type="Standard - Text" version="1" id="O203">
      <dia:attribute name="obj_pos">
        <dia:point val="12.7,19"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.7,18.735;13,19.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.7,19"/>
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
    <dia:object type="Standard - Text" version="1" id="O204">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4,19"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.4,18.735;13.7,19.0375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4,19"/>
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
    <dia:object type="Standard - Text" version="1" id="O205">
      <dia:attribute name="obj_pos">
        <dia:point val="11.3,19.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.3,19.29;13.75,19.6425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#送付件数(国保)#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.3,19.6"/>
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
    <dia:object type="Standard - Text" version="1" id="O206">
      <dia:attribute name="obj_pos">
        <dia:point val="11.3,20.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.3,19.99;13.75,20.3425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#送付件数(後期)#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.3,20.3"/>
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
    <dia:object type="Standard - Text" version="1" id="O207">
      <dia:attribute name="obj_pos">
        <dia:point val="18.6,19.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.6,18.835;18.9,19.1375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.6,19.1"/>
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
    <dia:object type="Standard - Text" version="1" id="O208">
      <dia:attribute name="obj_pos">
        <dia:point val="18.6,19.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.6,19.435;18.9,19.7375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.6,19.7"/>
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
    <dia:object type="Standard - Text" version="1" id="O209">
      <dia:attribute name="obj_pos">
        <dia:point val="18.6,20.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.6,20.135;18.9,20.4375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.6,20.4"/>
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
  <dia:layer name="差し込み" visible="true">
    <dia:object type="Embed - Text" version="1" id="O210">
      <dia:attribute name="obj_pos">
        <dia:point val="14.1,4.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.1,3.395;16.9,4.1975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#1234567#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="1"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.1,4.1"/>
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
    <dia:object type="Embed - Text" version="1" id="O211">
      <dia:attribute name="obj_pos">
        <dia:point val="6.7,4.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.08,4.33;6.7,4.7525"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#平成９９年９９月９９日#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52500000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.7,4.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIYMD#</dia:string>
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
        <dia:point val="11,5.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11,5.335;18.5,6.0125"/>
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
            <dia:point val="11,5.6"/>
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
    <dia:object type="Embed - Text" version="1" id="O213">
      <dia:attribute name="obj_pos">
        <dia:point val="11,6.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11,6.135;17,6.8125"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#いいいいいいいいいいいいいいいいいいいい
いいいいいいいいいいいいいいいいいいいい#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11,6.4"/>
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
    <dia:object type="Embed - Text" version="1" id="O214">
      <dia:attribute name="obj_pos">
        <dia:point val="11.6,9.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.65,9.465;11.6,9.8475"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.6,9.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HKN-TBL[0].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="15.9,9.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.24,9.465;15.9,9.8475"/>
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
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.9,9.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HKN-TBL[0].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="11.6,10.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.65,10.465;11.6,10.8475"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.6,10.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HKN-TBL[0].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="15.9,10.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.24,10.465;15.9,10.8475"/>
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
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.9,10.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HKN-TBL[0].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="6.1,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.15,17.565;6.1,17.9475"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,17.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#FUKUSI-TBL[0].F-KENSU[0]#</dia:string>
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
        <dia:point val="9,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.91,17.565;9,17.9475"/>
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
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9,17.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#FUKUSI-TBL[0].F-TENSU[0]#</dia:string>
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
        <dia:point val="6.1,18.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.15,18.465;6.1,18.8475"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,18.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#FUKUSI-TBL[0].F-KENSU[1]#</dia:string>
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
        <dia:point val="9,18.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.91,18.465;9,18.8475"/>
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
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9,18.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#FUKUSI-TBL[0].F-TENSU[1]#</dia:string>
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
        <dia:point val="6.1,19.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.15,19.365;6.1,19.7475"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,19.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#FUKUSI-TBL[0].F-KENSU[2]#</dia:string>
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
        <dia:point val="9,19.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.91,19.365;9,19.7475"/>
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
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9,19.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#FUKUSI-TBL[0].F-TENSU[2]#</dia:string>
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
        <dia:point val="6.1,20.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.15,20.365;6.1,20.7475"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,20.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#FUKUSI-TBL[0].F-KENSU[3]#</dia:string>
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
        <dia:point val="9,20.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.91,20.365;9,20.7475"/>
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
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9,20.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#FUKUSI-TBL[0].F-TENSU[3]#</dia:string>
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
        <dia:point val="6.1,21.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.15,21.365;6.1,21.7475"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,21.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#FUKUSI-TBL[1].F-KENSU[0]#</dia:string>
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
        <dia:point val="9,21.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.91,21.365;9,21.7475"/>
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
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9,21.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#FUKUSI-TBL[1].F-TENSU[0]#</dia:string>
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
        <dia:point val="6.1,22.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.15,22.265;6.1,22.6475"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,22.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#FUKUSI-TBL[1].F-KENSU[1]#</dia:string>
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
        <dia:point val="9,22.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.91,22.265;9,22.6475"/>
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
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9,22.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#FUKUSI-TBL[1].F-TENSU[1]#</dia:string>
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
        <dia:point val="11.6,11.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.65,11.365;11.6,11.7475"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.6,11.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#ETC-TBL[0].ETC-KENSU#</dia:string>
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
        <dia:point val="15.9,11.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.24,11.365;15.9,11.7475"/>
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
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.9,11.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#ETC-TBL[0].ETC-TENSU#</dia:string>
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
        <dia:point val="11.6,15.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.65,15.565;11.6,15.9475"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.6,15.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#ETC-TBL[1].ETC-KENSU#</dia:string>
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
        <dia:point val="15.9,15.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.24,15.565;15.9,15.9475"/>
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
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.9,15.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#ETC-TBL[1].ETC-TENSU#</dia:string>
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
        <dia:point val="11.6,14"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.65,13.665;11.6,14.0475"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.6,14"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HKN-TBL[1].NYUGAI-TBL[0].KENSU#</dia:string>
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
        <dia:point val="15.9,14"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.24,13.665;15.9,14.0475"/>
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
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.9,14"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HKN-TBL[1].NYUGAI-TBL[0].TENSU#</dia:string>
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
        <dia:point val="11.6,15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.65,14.665;11.6,15.0475"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.6,15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HKN-TBL[1].NYUGAI-TBL[1].KENSU#</dia:string>
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
        <dia:point val="15.9,15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.24,14.665;15.9,15.0475"/>
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
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.9,15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HKN-TBL[1].NYUGAI-TBL[1].TENSU#</dia:string>
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
        <dia:point val="7,10.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.05,10.365;7,10.7475"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7,10.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HKN-TBL[0].MAISU#</dia:string>
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
        <dia:point val="6.9,14.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.95,14.565;6.9,14.9475"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.9,14.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HKN-TBL[1].MAISU#</dia:string>
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
        <dia:point val="6.1,23.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.15,23.265;6.1,23.6475"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,23.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#FUKUSI-TBL[1].F-KENSU[2]#</dia:string>
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
        <dia:point val="9,23.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.91,23.265;9,23.6475"/>
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
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9,23.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#FUKUSI-TBL[1].F-TENSU[2]#</dia:string>
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
        <dia:point val="6.1,24.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.15,24.265;6.1,24.6475"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,24.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#FUKUSI-TBL[1].F-KENSU[3]#</dia:string>
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
        <dia:point val="9,24.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.91,24.265;9,24.6475"/>
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
            <dia:real val="0.47499999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9,24.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#FUKUSI-TBL[1].F-TENSU[3]#</dia:string>
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
        <dia:point val="9.675,5.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.675,5.3975;10.075,5.8"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#在#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.675,5.75"/>
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
        <dia:string>#ZAI1#</dia:string>
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
        <dia:point val="9.5375,5.9125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.5375,5.295;10.2375,5.9975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#○#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.875"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.5375,5.9125"/>
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
        <dia:string>#ZAI2#</dia:string>
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
        <dia:point val="9.33371,1.78696"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.33371,1.43446;11.3337,1.83696"/>
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
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.33371,1.78696"/>
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
  </dia:layer>
</dia:diagram>
