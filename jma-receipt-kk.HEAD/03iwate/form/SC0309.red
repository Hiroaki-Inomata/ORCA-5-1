<?xml version="1.0" encoding="UTF-8"?>
<dia:diagram xmlns:dia="http://www.lysator.liu.se/~alla/dia/">
  <dia:dictionarydata>
    <element name="KBN" occurs="3">
      <appinfo>
        <embed object="text" length="2"/>
      </appinfo>
    </element>
    <element name="SRYYM" occurs="1">
      <appinfo>
        <embed object="text" length="16"/>
      </appinfo>
    </element>
    <element name="HOSPCD" occurs="1">
      <appinfo>
        <embed object="text" length="20"/>
      </appinfo>
    </element>
    <element name="HOSPNAME" occurs="1">
      <appinfo>
        <embed object="text" length="100"/>
      </appinfo>
    </element>
    <element name="KENSU" occurs="1">
      <appinfo>
        <embed object="text" length="6"/>
      </appinfo>
    </element>
    <element name="TBL-CITY" occurs="33">
      <element name="CITY" occurs="1">
        <appinfo>
          <embed object="text" length="2"/>
        </appinfo>
      </element>
    </element>
    <element name="CITYKENSU" occurs="1">
      <appinfo>
        <embed object="text" length="3"/>
      </appinfo>
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
          <dia:real val="0.025000000000000001"/>
        </dia:attribute>
        <dia:attribute name="width_y">
          <dia:real val="0.025000000000000001"/>
        </dia:attribute>
        <dia:attribute name="visible_x">
          <dia:int val="2"/>
        </dia:attribute>
        <dia:attribute name="visible_y">
          <dia:int val="2"/>
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
    <dia:object type="Standard - Line" version="0" id="O0">
      <dia:attribute name="obj_pos">
        <dia:point val="2,6.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,6.485;2.015,13.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,6.5"/>
        <dia:point val="2,13"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O1">
      <dia:attribute name="obj_pos">
        <dia:point val="9,6.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,6.485;9.015,6.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="9,6.5"/>
        <dia:point val="2,6.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O2">
      <dia:attribute name="obj_pos">
        <dia:point val="9,8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.995,7.995;9.005,8.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="9,8"/>
        <dia:point val="2,8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O3">
      <dia:attribute name="obj_pos">
        <dia:point val="9,10"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,9.985;9.015,10.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="9,10"/>
        <dia:point val="2,10"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O4">
      <dia:attribute name="obj_pos">
        <dia:point val="9,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.995,11.495;9.005,11.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="9,11.5"/>
        <dia:point val="2,11.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O5">
      <dia:attribute name="obj_pos">
        <dia:point val="9,13"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,12.985;9.015,13.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="9,13"/>
        <dia:point val="2,13"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O6">
      <dia:attribute name="obj_pos">
        <dia:point val="8,6.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.985,6.485;8.015,13.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,6.5"/>
        <dia:point val="8,13"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O7">
      <dia:attribute name="obj_pos">
        <dia:point val="19,6.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.985,6.485;19.015,6.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,6.5"/>
        <dia:point val="9,6.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O8">
      <dia:attribute name="obj_pos">
        <dia:point val="19,13"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.985,12.985;19.015,13.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,13"/>
        <dia:point val="9,13"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O9">
      <dia:attribute name="obj_pos">
        <dia:point val="19,8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.995,7.995;19.005,8.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,8"/>
        <dia:point val="9,8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O10">
      <dia:attribute name="obj_pos">
        <dia:point val="19,10"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.985,9.985;19.015,10.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,10"/>
        <dia:point val="9,10"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O11">
      <dia:attribute name="obj_pos">
        <dia:point val="19,6.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.985,6.485;19.015,13.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,6.5"/>
        <dia:point val="19,13"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O12">
      <dia:attribute name="obj_pos">
        <dia:point val="19,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.995,11.495;19.005,11.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,11.5"/>
        <dia:point val="9,11.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O13">
      <dia:attribute name="obj_pos">
        <dia:point val="11,10"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.995,9.995;11.005,13.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11,10"/>
        <dia:point val="11,13"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O14">
      <dia:attribute name="obj_pos">
        <dia:point val="15,10"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.995,9.995;15.005,13.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15,10"/>
        <dia:point val="15,13"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O15">
      <dia:attribute name="obj_pos">
        <dia:point val="15,10"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.9936,9.99356;15.0064,11.5064"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15,10"/>
        <dia:point val="11,11.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="1" to="O13" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O16">
      <dia:attribute name="obj_pos">
        <dia:point val="15,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.9936,11.4936;15.0064,13.0064"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15,11.5"/>
        <dia:point val="11,13"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O14" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O17">
      <dia:attribute name="obj_pos">
        <dia:point val="19,10"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.9936,9.99356;19.0064,11.5064"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,10"/>
        <dia:point val="15,11.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="1" to="O14" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O18">
      <dia:attribute name="obj_pos">
        <dia:point val="19,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.9936,11.4936;19.0064,13.0064"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,11.5"/>
        <dia:point val="15,13"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O19">
      <dia:attribute name="obj_pos">
        <dia:point val="19,14"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,13.985;19.015,14.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,14"/>
        <dia:point val="2,14"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="3"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O20">
      <dia:attribute name="obj_pos">
        <dia:point val="19,27"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,26.985;19.015,27.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,27"/>
        <dia:point val="2,27"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="3"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O21">
      <dia:attribute name="obj_pos">
        <dia:point val="2,14"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,13.985;2.015,27.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,14"/>
        <dia:point val="2,27"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O22">
      <dia:attribute name="obj_pos">
        <dia:point val="19,14"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.985,13.985;19.015,27.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,14"/>
        <dia:point val="19,27"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O23">
      <dia:attribute name="obj_pos">
        <dia:point val="19,15.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,15.485;19.015,15.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,15.5"/>
        <dia:point val="2,15.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O24">
      <dia:attribute name="obj_pos">
        <dia:point val="19,25.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,25.385;19.015,25.415"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,25.4"/>
        <dia:point val="2,25.4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O25">
      <dia:attribute name="obj_pos">
        <dia:point val="10.5,25.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.485,25.385;10.515,27.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.5,25.4"/>
        <dia:point val="10.5,27"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O24" connection="0"/>
        <dia:connection handle="1" to="O20" connection="1"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O26">
      <dia:attribute name="obj_pos">
        <dia:point val="2,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,2.485;2.015,4.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,2.5"/>
        <dia:point val="2,4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O27">
      <dia:attribute name="obj_pos">
        <dia:point val="6,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.985,2.485;6.015,4.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6,2.5"/>
        <dia:point val="6,4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O28">
      <dia:attribute name="obj_pos">
        <dia:point val="6,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,2.485;6.015,2.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6,2.5"/>
        <dia:point val="2,2.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O29">
      <dia:attribute name="obj_pos">
        <dia:point val="6,4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,3.985;6.015,4.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6,4"/>
        <dia:point val="2,4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O30">
      <dia:attribute name="obj_pos">
        <dia:point val="4,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.995,2.495;4.005,4.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4,2.5"/>
        <dia:point val="4,4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O28" connection="0"/>
        <dia:connection handle="1" to="O29" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O31">
      <dia:attribute name="obj_pos">
        <dia:point val="3,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.995,2.495;3.005,4.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3,2.5"/>
        <dia:point val="3,4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O32">
      <dia:attribute name="obj_pos">
        <dia:point val="7,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.985,2.485;7.015,4.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="7,2.5"/>
        <dia:point val="7,4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O33">
      <dia:attribute name="obj_pos">
        <dia:point val="19,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.985,2.485;19.015,2.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,2.5"/>
        <dia:point val="7,2.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O34">
      <dia:attribute name="obj_pos">
        <dia:point val="19,4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.985,3.985;19.015,4.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,4"/>
        <dia:point val="7,4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O35">
      <dia:attribute name="obj_pos">
        <dia:point val="19,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.985,2.485;19.015,4.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,2.5"/>
        <dia:point val="19,4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O36">
      <dia:attribute name="obj_pos">
        <dia:point val="11,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.985,2.485;11.015,4.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11,2.5"/>
        <dia:point val="11,4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O33" connection="1"/>
        <dia:connection handle="1" to="O34" connection="1"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O37">
      <dia:attribute name="obj_pos">
        <dia:point val="15,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.985,2.485;15.015,4.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15,2.5"/>
        <dia:point val="15,4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O33" connection="0"/>
        <dia:connection handle="1" to="O34" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O38">
      <dia:attribute name="obj_pos">
        <dia:point val="8,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.995,2.495;8.005,4.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,2.5"/>
        <dia:point val="8,4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O39">
      <dia:attribute name="obj_pos">
        <dia:point val="12,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.995,2.495;12.005,4.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12,2.5"/>
        <dia:point val="12,4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O40">
      <dia:attribute name="obj_pos">
        <dia:point val="16,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.995,2.495;16.005,4.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="16,2.5"/>
        <dia:point val="16,4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
  </dia:layer>
  <dia:layer name="文字" visible="true">
    <dia:object type="Standard - Text" version="1" id="O41">
      <dia:attribute name="obj_pos">
        <dia:point val="9.5,5.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.5,4.8775;17.96,5.585"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#医療費助成事業提出総括票#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.88194441050291061"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.5,5.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Standard - Text" version="1" id="O42">
      <dia:attribute name="obj_pos">
        <dia:point val="2.475,9.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.475,8.7025;7.55,9.2125"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#保険医療機関等の名称#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.63499997556209564"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.475,9.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Standard - Text" version="1" id="O43">
      <dia:attribute name="obj_pos">
        <dia:point val="2.475,10.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.475,10.5025;7.5575,11.0125"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#提 出 市 町 村 集 計#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.63499997556209564"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.475,10.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Standard - Text" version="1" id="O44">
      <dia:attribute name="obj_pos">
        <dia:point val="2.475,12.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.475,12.0025;7.55,12.5125"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#合　　　　　　　　計#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.63499997556209564"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.475,12.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Standard - Text" version="1" id="O45">
      <dia:attribute name="obj_pos">
        <dia:point val="9,10.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9,10.5025;10.015,11.0125"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.63499997556209564"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9,10.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Standard - Text" version="1" id="O46">
      <dia:attribute name="obj_pos">
        <dia:point val="2.15,3.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.15,2.9275;2.855,3.635"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#０#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.88194441050291061"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.15,3.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Standard - Text" version="1" id="O47">
      <dia:attribute name="obj_pos">
        <dia:point val="3.15,3.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.15,2.9275;3.855,3.635"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#３#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.88194441050291061"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.15,3.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="4.3,3.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.3,2.9275;5.71,3.635"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#岩手#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.88194441050291061"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.3,3.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="8.2,5.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.2,4.8775;8.905,5.585"/>
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
            <dia:real val="0.88194441050291061"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.2,5.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="5.15,14.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.15,14.5025;15.8075,15.0125"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#提　　　　出　　　　市　　　　町　　　　村#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.63499997556209564"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.15,14.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="3.4,26.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.4,25.9525;8.9825,26.4625"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#提　出　市　町　村　数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.63499997556209564"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.4,26.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="8.8,3.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8,3.0025;10.2125,3.57"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#現 物#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.70555552840232849"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.8,3.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="12.8,3.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.8,2.7025;14.2125,3.27"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#償 還#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.70555552840232849"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.8,3.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="12.4,3.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.4,3.3025;14.66,3.87"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（県単）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.70555552840232849"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.4,3.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="16.8,3.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.8,2.7025;18.2125,3.27"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#償 還#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.70555552840232849"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.8,3.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="16.4,3.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.4,3.3025;18.66,3.87"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（市単）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.70555552840232849"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.4,3.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="15.75,16.225"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.75,15.8525;17.8625,16.2775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#普　代　村#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.75,16.225"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="15.75,17.325"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.75,16.9525;17.8625,17.3775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#軽　米　町#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.75,17.325"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="15.75,18.425"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.75,18.0525;17.8625,18.4775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#洋　野　町#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.75,18.425"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="15.75,19.525"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.75,19.1525;17.8625,19.5775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#野　田　村#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.75,19.525"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="15.75,20.625"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.75,20.2525;17.8625,20.6775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#九　戸　村#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.75,20.625"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="15.75,21.725"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.75,21.3525;17.8625,21.7775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#一　戸　町#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.75,21.725"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="11.5,16.225"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.5,15.8525;13.6125,16.2775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#矢　巾　町#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.5,16.225"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="11.5,19.525"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.5,19.1525;13.6125,19.5775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#平　泉　町#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.5,19.525"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="11.5,20.625"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.5,20.2525;13.6125,20.6775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#住　田　町#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.5,20.625"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="11.5,21.725"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.5,21.3525;13.6125,21.7775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#大　槌　町#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.5,21.725"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="11.5,22.825"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.5,22.4525;13.6125,22.8775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#山　田　町#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.5,22.825"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="11.5,23.925"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.5,23.5525;13.6125,23.9775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#岩　泉　町#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.5,23.925"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:group>
      <dia:object type="Standard - Text" version="1" id="O69">
        <dia:attribute name="obj_pos">
          <dia:point val="11.5,17.325"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.5,16.9525;11.9225,17.3775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#西#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.5,17.325"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="12.6333,17.325"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="12.6333,16.9525;13.0558,17.3775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#賀#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="12.6333,17.325"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="13.2,17.325"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="13.2,16.9525;13.6225,17.3775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#町#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="13.2,17.325"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="12.0667,17.325"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="12.0667,16.9525;12.4892,17.3775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#和#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="12.0667,17.325"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="1" id="O73">
        <dia:attribute name="obj_pos">
          <dia:point val="11.5,18.425"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.5,18.0525;11.9225,18.4775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#金#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.5,18.425"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="12.0625,18.425"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="12.0625,18.0525;12.485,18.4775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ヶ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="12.0625,18.425"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="13.1875,18.425"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="13.1875,18.0525;13.61,18.4775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#町#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="13.1875,18.425"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="12.625,18.425"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="12.625,18.0525;13.0475,18.4775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#崎#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="12.625,18.425"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
    </dia:group>
    <dia:object type="Standard - Text" version="1" id="O77">
      <dia:attribute name="obj_pos">
        <dia:point val="7.25,16.225"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.25,15.8525;9.3625,16.2775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#陸前高田市#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.25,16.225"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="7.25,17.325"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.25,16.9525;9.3625,17.3775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#釜　石　市#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.25,17.325"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="7.25,18.425"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.25,18.0525;9.3625,18.4775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#二　戸　市#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.25,18.425"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="7.25,20.625"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.25,20.2525;9.3625,20.6775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#滝　沢　市#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.25,20.625"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="7.25,21.725"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.25,21.3525;9.3625,21.7775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#雫　石　町#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.25,21.725"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="7.25,22.825"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.25,22.4525;9.3625,22.8775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#葛　巻　町#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.25,22.825"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="7.25,23.925"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.25,23.5525;9.3625,23.9775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#岩　手　町#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.25,23.925"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="7.25,25.0505"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.25,24.678;9.3625,25.103"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#紫　波　町#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.25,25.0505"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:group>
      <dia:object type="Standard - Text" version="1" id="O85">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,19.525"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="7.25,19.1525;7.6725,19.5775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#八#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,19.525"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="7.8125,19.525"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="7.8125,19.1525;8.235,19.5775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#幡#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.8125,19.525"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="8.375,19.525"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="8.375,19.1525;8.7975,19.5775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#平#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="8.375,19.525"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="8.9375,19.525"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="8.9375,19.1525;9.36,19.5775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#市#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="8.9375,19.525"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
    </dia:group>
    <dia:object type="Standard - Text" version="1" id="O89">
      <dia:attribute name="obj_pos">
        <dia:point val="3,16.225"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3,15.8525;5.1125,16.2775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#盛　岡　市#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,16.225"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="3,17.325"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3,16.9525;5.1125,17.3775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#宮　古　市#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,17.325"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="3,19.525"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3,19.1525;5.1125,19.5775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#奥　州　市#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,19.525"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="3,20.625"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3,20.2525;5.1125,20.6775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#花　巻　市#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,20.625"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="3,21.725"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3,21.3525;5.1125,21.7775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#北　上　市#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,21.725"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="3,22.825"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3,22.4525;5.1125,22.8775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#久　慈　市#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,22.825"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="3,23.925"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3,23.5525;5.1125,23.9775"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#遠　野　市#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,23.925"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:point val="3,25.0505"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3,24.678;5.1125,25.103"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#一　関　市#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3,25.0505"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:group>
      <dia:object type="Standard - Text" version="1" id="O97">
        <dia:attribute name="obj_pos">
          <dia:point val="3,18.425"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3,18.0525;3.4225,18.4775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#大#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="3,18.425"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="3.5625,18.425"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3.5625,18.0525;3.985,18.4775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#船#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="3.5625,18.425"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="4.125,18.425"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.125,18.0525;4.5475,18.4775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#渡#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.125,18.425"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="4.6875,18.425"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.6875,18.0525;5.11,18.4775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#市#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.6875,18.425"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="1" id="O101">
        <dia:attribute name="obj_pos">
          <dia:point val="2.475,7.45"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.475,7.0025;2.9825,7.5125"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#医#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.63499997556209564"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.475,7.45"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="3.12321,7.45"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3.12321,7.0025;3.63071,7.5125"/>
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
              <dia:real val="0.63499997556209564"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="3.12321,7.45"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="3.77143,7.45"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3.77143,7.0025;4.27893,7.5125"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#機#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.63499997556209564"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="3.77143,7.45"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="4.41964,7.45"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.41964,7.0025;4.92714,7.5125"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#関#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.63499997556209564"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.41964,7.45"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="5.06786,7.45"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="5.06786,7.0025;5.57536,7.5125"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#等#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.63499997556209564"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="5.06786,7.45"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="5.71607,7.45"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="5.71607,7.0025;6.22357,7.5125"/>
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
              <dia:real val="0.63499997556209564"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="5.71607,7.45"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="6.36429,7.45"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.36429,7.0025;6.87179,7.5125"/>
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
              <dia:real val="0.63499997556209564"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.36429,7.45"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="7.0125,7.45"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="7.0125,7.0025;7.52,7.5125"/>
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
              <dia:real val="0.63499997556209564"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.0125,7.45"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="1" id="O109">
        <dia:attribute name="obj_pos">
          <dia:point val="11.5,25.025"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.5,24.6525;11.9225,25.0775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#田#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.5,25.025"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="12.0625,25.025"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="12.0625,24.6525;12.485,25.0775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#野#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="12.0625,25.025"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="12.625,25.025"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="12.625,24.6525;13.0475,25.0775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#畑#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="12.625,25.025"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:point val="13.1875,25.025"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="13.1875,24.6525;13.61,25.0775"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#村#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.52916664630174637"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="13.1875,25.025"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
    </dia:group>
  </dia:layer>
  <dia:layer name="差し込み" visible="true" active="true">
    <dia:object type="Embed - Text" version="1" id="O113">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,5.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5,4.8775;8.14,5.585"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#平成９９年９９月#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.88194441050291061"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.5,5.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#SRYYM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O114">
      <dia:attribute name="obj_pos">
        <dia:point val="8.35,7.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.35,6.9275;15.4,7.635"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９９９９９９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.88194441050291061"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.35,7.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O115">
      <dia:attribute name="obj_pos">
        <dia:point val="8.25,8.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.25,8.5775;18.8125,9.53167"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああいいいいいうううううえええええおおおおお
あああああいいいいいうううううえええええおおおおお#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.25,8.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O116">
      <dia:attribute name="obj_pos">
        <dia:point val="15.75,26.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.4825,25.855;15.75,26.7025"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="1.0583332926034927"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.75,26.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#CITYKENSU#</dia:string>
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
        <dia:point val="10.6,12.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.485,11.9275;10.6,12.635"/>
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
            <dia:real val="0.88194441050291061"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.6,12.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:group>
      <dia:object type="Embed - Text" version="1" id="O118">
        <dia:attribute name="obj_pos">
          <dia:point val="7.21875,3.46875"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="7.21875,2.97125;7.78375,3.53875"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.21875,3.46875"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#KBN[0]#</dia:string>
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
          <dia:point val="11.2188,3.46875"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.2188,2.97125;11.7838,3.53875"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.2188,3.46875"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#KBN[1]#</dia:string>
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
          <dia:point val="15.2188,3.46875"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="15.2188,2.97125;15.7838,3.53875"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="15.2188,3.46875"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#KBN[2]#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
    </dia:group>
    <dia:group>
      <dia:object type="Embed - Text" version="1" id="O121">
        <dia:attribute name="obj_pos">
          <dia:point val="2.225,16.275"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.225,15.7775;2.79,16.345"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.225,16.275"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[0].CITY#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O122">
        <dia:attribute name="obj_pos">
          <dia:point val="2.225,17.375"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.225,16.8775;2.79,17.445"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.225,17.375"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[1].CITY#</dia:string>
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
          <dia:point val="2.225,18.475"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.225,17.9775;2.79,18.545"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.225,18.475"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[2].CITY#</dia:string>
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
          <dia:point val="2.225,19.575"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.225,19.0775;2.79,19.645"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.225,19.575"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[3].CITY#</dia:string>
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
          <dia:point val="2.225,20.675"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.225,20.1775;2.79,20.745"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.225,20.675"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[4].CITY#</dia:string>
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
          <dia:point val="2.225,21.775"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.225,21.2775;2.79,21.845"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.225,21.775"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[5].CITY#</dia:string>
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
          <dia:point val="2.225,22.875"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.225,22.3775;2.79,22.945"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.225,22.875"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[6].CITY#</dia:string>
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
          <dia:point val="2.225,23.975"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.225,23.4775;2.79,24.045"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.225,23.975"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[7].CITY#</dia:string>
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
          <dia:point val="2.225,25.075"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.225,24.5775;2.79,25.145"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.225,25.075"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[8].CITY#</dia:string>
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
          <dia:point val="6.475,16.275"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.475,15.7775;7.04,16.345"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.475,16.275"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[9].CITY#</dia:string>
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
          <dia:point val="6.46081,17.375"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.46081,16.8775;7.02581,17.445"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.46081,17.375"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[10].CITY#</dia:string>
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
          <dia:point val="6.46081,18.475"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.46081,17.9775;7.02581,18.545"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.46081,18.475"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[11].CITY#</dia:string>
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
          <dia:point val="6.46081,19.575"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.46081,19.0775;7.02581,19.645"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.46081,19.575"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[12].CITY#</dia:string>
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
          <dia:point val="6.46081,20.675"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.46081,20.1775;7.02581,20.745"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.46081,20.675"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[13].CITY#</dia:string>
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
          <dia:point val="6.46081,21.775"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.46081,21.2775;7.02581,21.845"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.46081,21.775"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[14].CITY#</dia:string>
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
          <dia:point val="6.46081,22.875"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.46081,22.3775;7.02581,22.945"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.46081,22.875"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[15].CITY#</dia:string>
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
          <dia:point val="6.46081,23.975"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.46081,23.4775;7.02581,24.045"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.46081,23.975"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[16].CITY#</dia:string>
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
          <dia:point val="6.46081,25.075"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.46081,24.5775;7.02581,25.145"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.46081,25.075"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[17].CITY#</dia:string>
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
          <dia:point val="10.725,16.275"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="10.725,15.7775;11.29,16.345"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="10.725,16.275"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[18].CITY#</dia:string>
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
          <dia:point val="10.7108,17.375"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="10.7108,16.8775;11.2758,17.445"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="10.7108,17.375"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[19].CITY#</dia:string>
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
          <dia:point val="10.7108,18.475"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="10.7108,17.9775;11.2758,18.545"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="10.7108,18.475"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[20].CITY#</dia:string>
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
          <dia:point val="10.7108,19.575"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="10.7108,19.0775;11.2758,19.645"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="10.7108,19.575"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[21].CITY#</dia:string>
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
          <dia:point val="10.7108,20.675"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="10.7108,20.1775;11.2758,20.745"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="10.7108,20.675"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[22].CITY#</dia:string>
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
          <dia:point val="10.7108,21.775"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="10.7108,21.2775;11.2758,21.845"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="10.7108,21.775"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[23].CITY#</dia:string>
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
          <dia:point val="10.7108,22.875"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="10.7108,22.3775;11.2758,22.945"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="10.7108,22.875"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[24].CITY#</dia:string>
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
          <dia:point val="10.7108,23.975"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="10.7108,23.4775;11.2758,24.045"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="10.7108,23.975"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[25].CITY#</dia:string>
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
          <dia:point val="10.7108,25.075"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="10.7108,24.5775;11.2758,25.145"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="10.7108,25.075"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[26].CITY#</dia:string>
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
          <dia:point val="14.95,16.275"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="14.95,15.7775;15.515,16.345"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="14.95,16.275"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[27].CITY#</dia:string>
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
          <dia:point val="14.9358,17.375"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="14.9358,16.8775;15.5008,17.445"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="14.9358,17.375"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[28].CITY#</dia:string>
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
          <dia:point val="14.9358,18.475"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="14.9358,17.9775;15.5008,18.545"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="14.9358,18.475"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[29].CITY#</dia:string>
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
          <dia:point val="14.9358,19.575"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="14.9358,19.0775;15.5008,19.645"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="14.9358,19.575"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[30].CITY#</dia:string>
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
          <dia:point val="14.9358,20.675"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="14.9358,20.1775;15.5008,20.745"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="14.9358,20.675"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[31].CITY#</dia:string>
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
          <dia:point val="14.9358,21.775"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="14.9358,21.2775;15.5008,21.845"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#□#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.70555552840232849"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="14.9358,21.775"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#TBL-CITY[32].CITY#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
    </dia:group>
  </dia:layer>
</dia:diagram>
