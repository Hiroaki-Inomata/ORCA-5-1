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
    <element name="TENHYOKBN" occurs="4">
      <appinfo>
        <embed object="text" length="2"/>
      </appinfo>
    </element>
    <element name="HOSPCD" occurs="1">
      <appinfo>
        <embed object="text" length="20"/>
      </appinfo>
    </element>
    <element name="CITYNAME" occurs="1">
      <appinfo>
        <embed object="text" length="10"/>
      </appinfo>
    </element>
    <element name="CITYNUM-TBL" occurs="1">
      <element name="CITYNUM" occurs="2">
        <appinfo>
          <embed object="text" length="2"/>
        </appinfo>
      </element>
    </element>
    <element name="HOSPADRS" occurs="1">
      <appinfo>
        <embed object="text" length="100"/>
      </appinfo>
    </element>
    <element name="HOSPNAME" occurs="1">
      <appinfo>
        <embed object="text" length="100"/>
      </appinfo>
    </element>
    <element name="HOSPTEL" occurs="1">
      <appinfo>
        <embed object="text" length="15"/>
      </appinfo>
    </element>
    <element name="SEIKYU-TBL" occurs="2">
      <element name="KENSU" occurs="1">
        <appinfo>
          <embed object="text" length="5"/>
        </appinfo>
      </element>
      <element name="TENSU" occurs="1">
        <appinfo>
          <embed object="text" length="10"/>
        </appinfo>
      </element>
      <element name="ITBFTN" occurs="1">
        <appinfo>
          <embed object="text" length="9"/>
        </appinfo>
      </element>
    </element>
    <element name="TOKKI-TBL" occurs="15">
      <element name="TOKKI-NYUGAI-TBL" occurs="2">
        <element name="TOKKIKENSU" occurs="1">
          <appinfo>
            <embed object="text" length="3"/>
          </appinfo>
        </element>
      </element>
    </element>
    <element name="KOHI-TBL" occurs="15">
      <element name="KOHI-NYUGAI-TBL" occurs="2">
        <element name="KOHIKENSU" occurs="1">
          <appinfo>
            <embed object="text" length="3"/>
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
          <dia:real val="0.050000000000000003"/>
        </dia:attribute>
        <dia:attribute name="width_y">
          <dia:real val="0.050000000000000003"/>
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
        <dia:point val="2,7.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,7.485;2.015,9.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,7.5"/>
        <dia:point val="2,9.5"/>
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
        <dia:point val="8,5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,4.985;8.015,5.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,5"/>
        <dia:point val="2,5"/>
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
        <dia:point val="19,5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.985,4.985;19.015,5.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,5"/>
        <dia:point val="8,5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O3">
      <dia:attribute name="obj_pos">
        <dia:point val="2,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,2.485;2.015,3.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,2.5"/>
        <dia:point val="2,3.5"/>
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
        <dia:point val="5,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.985,2.485;5.015,3.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5,2.5"/>
        <dia:point val="5,3.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O5">
      <dia:attribute name="obj_pos">
        <dia:point val="5,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,2.485;5.015,2.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5,2.5"/>
        <dia:point val="2,2.5"/>
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
        <dia:point val="5,3.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,3.485;5.015,3.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5,3.5"/>
        <dia:point val="2,3.5"/>
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
        <dia:point val="3.5,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.495,2.495;3.505,3.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.5,2.5"/>
        <dia:point val="3.5,3.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O5" connection="0"/>
        <dia:connection handle="1" to="O6" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O8">
      <dia:attribute name="obj_pos">
        <dia:point val="2.75,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.745,2.495;2.755,3.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.75,2.5"/>
        <dia:point val="2.75,3.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O9">
      <dia:attribute name="obj_pos">
        <dia:point val="11.5,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.485,2.485;11.515,3.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.5,2.5"/>
        <dia:point val="11.5,3.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O10">
      <dia:attribute name="obj_pos">
        <dia:point val="19,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.485,2.485;19.015,2.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,2.5"/>
        <dia:point val="11.5,2.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O11">
      <dia:attribute name="obj_pos">
        <dia:point val="19,3.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.485,3.485;19.015,3.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,3.5"/>
        <dia:point val="11.5,3.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O12">
      <dia:attribute name="obj_pos">
        <dia:point val="19,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.985,2.485;19.015,3.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,2.5"/>
        <dia:point val="19,3.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O13">
      <dia:attribute name="obj_pos">
        <dia:point val="14,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.985,2.485;14.015,3.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="14,2.5"/>
        <dia:point val="14,3.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O10" connection="1"/>
        <dia:connection handle="1" to="O11" connection="1"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O14">
      <dia:attribute name="obj_pos">
        <dia:point val="16.5,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.485,2.485;16.515,3.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="16.5,2.5"/>
        <dia:point val="16.5,3.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O10" connection="0"/>
        <dia:connection handle="1" to="O11" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O15">
      <dia:attribute name="obj_pos">
        <dia:point val="12.1,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.095,2.495;12.105,3.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.1,2.5"/>
        <dia:point val="12.1,3.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O16">
      <dia:attribute name="obj_pos">
        <dia:point val="14.6,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.595,2.495;14.605,3.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="14.6,2.5"/>
        <dia:point val="14.6,3.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O17">
      <dia:attribute name="obj_pos">
        <dia:point val="17.1,2.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.095,2.495;17.105,3.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.1,2.5"/>
        <dia:point val="17.1,3.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O18">
      <dia:attribute name="obj_pos">
        <dia:point val="2,5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,4.985;2.015,7.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,5"/>
        <dia:point val="2,7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O19">
      <dia:attribute name="obj_pos">
        <dia:point val="8,5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.985,4.985;8.015,7.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,5"/>
        <dia:point val="8,7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O20">
      <dia:attribute name="obj_pos">
        <dia:point val="19,5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.985,4.985;19.015,7.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,5"/>
        <dia:point val="19,7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O21">
      <dia:attribute name="obj_pos">
        <dia:point val="8,5.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.995,5.495;8.005,5.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,5.5"/>
        <dia:point val="2,5.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O22">
      <dia:attribute name="obj_pos">
        <dia:point val="8,5.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.995,5.895;8.005,5.905"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,5.9"/>
        <dia:point val="2,5.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O23">
      <dia:attribute name="obj_pos">
        <dia:point val="19,7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.985,6.985;19.015,7.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,7"/>
        <dia:point val="8,7"/>
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
        <dia:point val="19,5.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.995,5.495;19.005,5.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,5.5"/>
        <dia:point val="8,5.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O25">
      <dia:attribute name="obj_pos">
        <dia:point val="8,7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,6.985;8.015,7.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,7"/>
        <dia:point val="2,7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O26">
      <dia:attribute name="obj_pos">
        <dia:point val="3.5,5.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.495,5.495;3.505,7.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.5,5.5"/>
        <dia:point val="3.5,7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O27">
      <dia:attribute name="obj_pos">
        <dia:point val="5,5.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.995,5.495;5.005,7.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5,5.5"/>
        <dia:point val="5,7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O21" connection="0"/>
        <dia:connection handle="1" to="O25" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O28">
      <dia:attribute name="obj_pos">
        <dia:point val="6.5,5.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.495,5.495;6.505,7.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6.5,5.5"/>
        <dia:point val="6.5,7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O29">
      <dia:attribute name="obj_pos">
        <dia:point val="8,7.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,7.485;8.015,7.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,7.5"/>
        <dia:point val="2,7.5"/>
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
        <dia:point val="8,9.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,9.485;8.015,9.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,9.5"/>
        <dia:point val="2,9.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O31">
      <dia:attribute name="obj_pos">
        <dia:point val="8,7.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.985,7.485;8.015,9.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,7.5"/>
        <dia:point val="8,9.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O32">
      <dia:attribute name="obj_pos">
        <dia:point val="5,7.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.985,7.485;5.015,9.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5,7.5"/>
        <dia:point val="5,9.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O29" connection="0"/>
        <dia:connection handle="1" to="O30" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O33">
      <dia:attribute name="obj_pos">
        <dia:point val="8,8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.995,7.995;8.005,8.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,8"/>
        <dia:point val="2,8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O34">
      <dia:attribute name="obj_pos">
        <dia:point val="6,8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.995,7.995;6.005,9.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6,8"/>
        <dia:point val="6,9.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O35">
      <dia:attribute name="obj_pos">
        <dia:point val="7,8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.995,7.995;7.005,9.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="7,8"/>
        <dia:point val="7,9.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O36">
      <dia:attribute name="obj_pos">
        <dia:point val="8.5,7.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.485,7.485;8.515,11.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.5,7.5"/>
        <dia:point val="8.5,11"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O37">
      <dia:attribute name="obj_pos">
        <dia:point val="19,7.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.985,7.485;19.015,11.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,7.5"/>
        <dia:point val="19,11"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O38">
      <dia:attribute name="obj_pos">
        <dia:point val="19,7.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.485,7.485;19.015,7.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,7.5"/>
        <dia:point val="8.5,7.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O39">
      <dia:attribute name="obj_pos">
        <dia:point val="19,11"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.485,10.985;19.015,11.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,11"/>
        <dia:point val="8.5,11"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O40">
      <dia:attribute name="obj_pos">
        <dia:point val="19,8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.495,7.995;19.005,8.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,8"/>
        <dia:point val="8.5,8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O41">
      <dia:attribute name="obj_pos">
        <dia:point val="19,10.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.495,10.395;19.005,10.405"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,10.4"/>
        <dia:point val="8.5,10.4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O42">
      <dia:attribute name="obj_pos">
        <dia:point val="11,10.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.995,10.395;11.005,11.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11,10.4"/>
        <dia:point val="11,11"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O43">
      <dia:attribute name="obj_pos">
        <dia:point val="2,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,11.485;2.015,14.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,11.5"/>
        <dia:point val="2,14"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O44">
      <dia:attribute name="obj_pos">
        <dia:point val="19,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.985,11.485;19.015,11.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,11.5"/>
        <dia:point val="5,11.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O45">
      <dia:attribute name="obj_pos">
        <dia:point val="19,14"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.985,13.985;19.015,14.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,14"/>
        <dia:point val="5,14"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O46">
      <dia:attribute name="obj_pos">
        <dia:point val="5,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.985,11.485;5.015,14.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5,11.5"/>
        <dia:point val="5,14"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O47">
      <dia:attribute name="obj_pos">
        <dia:point val="19,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.985,11.485;19.015,14.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,11.5"/>
        <dia:point val="19,14"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O48">
      <dia:attribute name="obj_pos">
        <dia:point val="5,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,11.485;5.015,11.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5,11.5"/>
        <dia:point val="2,11.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O49">
      <dia:attribute name="obj_pos">
        <dia:point val="5,14"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,13.985;5.015,14.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5,14"/>
        <dia:point val="2,14"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O50">
      <dia:attribute name="obj_pos">
        <dia:point val="5,12.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.995,12.745;5.005,12.755"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5,12.75"/>
        <dia:point val="2,12.75"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O46" connection="0"/>
        <dia:connection handle="1" to="O43" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O51">
      <dia:attribute name="obj_pos">
        <dia:point val="19,12.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.995,12.745;19.005,12.755"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,12.75"/>
        <dia:point val="5,12.75"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O47" connection="0"/>
        <dia:connection handle="1" to="O46" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O52">
      <dia:attribute name="obj_pos">
        <dia:point val="19,12.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.995,12.145;19.005,12.155"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,12.15"/>
        <dia:point val="5,12.15"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O53">
      <dia:attribute name="obj_pos">
        <dia:point val="6.2,12.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.195,12.145;6.205,14.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6.2,12.15"/>
        <dia:point val="6.2,14"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O54">
      <dia:attribute name="obj_pos">
        <dia:point val="12,11.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.985,11.485;12.015,14.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12,11.5"/>
        <dia:point val="12,14"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O44" connection="0"/>
        <dia:connection handle="1" to="O45" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O55">
      <dia:attribute name="obj_pos">
        <dia:point val="9.2,12.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.195,12.145;9.205,14.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="9.2,12.15"/>
        <dia:point val="9.2,14"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O56">
      <dia:attribute name="obj_pos">
        <dia:point val="2,21.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,21.185;2.015,27.615"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,21.2"/>
        <dia:point val="2,27.6"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O57">
      <dia:attribute name="obj_pos">
        <dia:point val="19,21.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,21.185;19.015,21.215"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,21.2"/>
        <dia:point val="2,21.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O58">
      <dia:attribute name="obj_pos">
        <dia:point val="19,22.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.995,22.795;19.005,22.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,22.8"/>
        <dia:point val="2,22.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O59">
      <dia:attribute name="obj_pos">
        <dia:point val="19,23.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.995,23.595;19.005,23.605"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,23.6"/>
        <dia:point val="2,23.6"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O60">
      <dia:attribute name="obj_pos">
        <dia:point val="19,24.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.995,24.395;19.005,24.405"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,24.4"/>
        <dia:point val="2,24.4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O61">
      <dia:attribute name="obj_pos">
        <dia:point val="19,25.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.995,25.195;19.005,25.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,25.2"/>
        <dia:point val="2,25.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O62">
      <dia:attribute name="obj_pos">
        <dia:point val="15.6,26"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.995,25.995;15.605,26.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.6,26"/>
        <dia:point val="2,26"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O63">
      <dia:attribute name="obj_pos">
        <dia:point val="15.6,26.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.995,26.795;15.605,26.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.6,26.8"/>
        <dia:point val="2,26.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O64">
      <dia:attribute name="obj_pos">
        <dia:point val="15.6,27.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,27.585;15.615,27.615"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.6,27.6"/>
        <dia:point val="2,27.6"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O65">
      <dia:attribute name="obj_pos">
        <dia:point val="19,21.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.985,21.185;19.015,26.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,21.2"/>
        <dia:point val="19,26"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O66">
      <dia:attribute name="obj_pos">
        <dia:point val="19,22"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,21.985;19.015,22.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,22"/>
        <dia:point val="2,22"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="4"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O67">
      <dia:attribute name="obj_pos">
        <dia:point val="5.4,22"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.395,21.995;5.405,27.605"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.4,22"/>
        <dia:point val="5.4,27.6"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O66" connection="3"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O68">
      <dia:attribute name="obj_pos">
        <dia:point val="8.8,22"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.795,21.995;8.805,27.605"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.8,22"/>
        <dia:point val="8.8,27.6"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O66" connection="2"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O69">
      <dia:attribute name="obj_pos">
        <dia:point val="12.2,22"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.195,21.995;12.205,27.605"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.2,22"/>
        <dia:point val="12.2,27.6"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O66" connection="1"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O70">
      <dia:attribute name="obj_pos">
        <dia:point val="15.6,22"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.595,21.995;15.605,26.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.6,22"/>
        <dia:point val="15.6,26"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O66" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O71">
      <dia:attribute name="obj_pos">
        <dia:point val="2,14.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,14.485;2.015,20.715"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,14.5"/>
        <dia:point val="2,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O72">
      <dia:attribute name="obj_pos">
        <dia:point val="19,14.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,14.485;19.015,14.515"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,14.5"/>
        <dia:point val="2,14.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O73">
      <dia:attribute name="obj_pos">
        <dia:point val="19,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.985,15.185;19.015,15.215"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,15.2"/>
        <dia:point val="4,15.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O74">
      <dia:attribute name="obj_pos">
        <dia:point val="19,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.995,15.695;19.005,15.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,15.7"/>
        <dia:point val="4,15.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O75">
      <dia:attribute name="obj_pos">
        <dia:point val="19,17.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.995,17.195;19.005,17.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,17.2"/>
        <dia:point val="4,17.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O76">
      <dia:attribute name="obj_pos">
        <dia:point val="19,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,18.185;19.015,18.215"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,18.2"/>
        <dia:point val="2,18.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O77">
      <dia:attribute name="obj_pos">
        <dia:point val="19,14.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.985,14.485;19.015,20.715"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,14.5"/>
        <dia:point val="19,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O78">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.485,15.185;2.515,18.215"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,15.2"/>
        <dia:point val="2.5,18.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O79">
      <dia:attribute name="obj_pos">
        <dia:point val="4,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.985,15.185;4.015,18.215"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4,15.2"/>
        <dia:point val="4,18.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O80">
      <dia:attribute name="obj_pos">
        <dia:point val="2,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,15.185;4.015,15.215"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,15.2"/>
        <dia:point val="4,15.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O81">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,16.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.495,16.195;4.005,16.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,16.2"/>
        <dia:point val="4,16.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O82">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,17.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.495,17.195;4.005,17.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,17.2"/>
        <dia:point val="4,17.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O83">
      <dia:attribute name="obj_pos">
        <dia:point val="19,16.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.995,16.195;19.005,16.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,16.2"/>
        <dia:point val="4,16.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O84">
      <dia:attribute name="obj_pos">
        <dia:point val="5,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.995,15.695;5.005,18.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5,15.7"/>
        <dia:point val="5,18.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O85">
      <dia:attribute name="obj_pos">
        <dia:point val="6,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.995,15.195;6.005,18.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6,15.2"/>
        <dia:point val="6,18.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O86">
      <dia:attribute name="obj_pos">
        <dia:point val="7,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.995,15.695;7.005,18.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="7,15.7"/>
        <dia:point val="7,18.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O87">
      <dia:attribute name="obj_pos">
        <dia:point val="8,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.995,15.695;8.005,18.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,15.7"/>
        <dia:point val="8,18.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O88">
      <dia:attribute name="obj_pos">
        <dia:point val="9,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.995,15.195;9.005,18.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="9,15.2"/>
        <dia:point val="9,18.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O89">
      <dia:attribute name="obj_pos">
        <dia:point val="10,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.995,15.695;10.005,18.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10,15.7"/>
        <dia:point val="10,18.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O90">
      <dia:attribute name="obj_pos">
        <dia:point val="11,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.995,15.695;11.005,18.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11,15.7"/>
        <dia:point val="11,18.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O91">
      <dia:attribute name="obj_pos">
        <dia:point val="12,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.995,15.695;12.005,18.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12,15.7"/>
        <dia:point val="12,18.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O92">
      <dia:attribute name="obj_pos">
        <dia:point val="13,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.995,15.695;13.005,18.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13,15.7"/>
        <dia:point val="13,18.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O93">
      <dia:attribute name="obj_pos">
        <dia:point val="14,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.995,15.195;14.005,18.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="14,15.2"/>
        <dia:point val="14,18.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O94">
      <dia:attribute name="obj_pos">
        <dia:point val="15,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.995,15.695;15.005,18.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15,15.7"/>
        <dia:point val="15,18.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O95">
      <dia:attribute name="obj_pos">
        <dia:point val="16,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.995,15.695;16.005,18.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="16,15.7"/>
        <dia:point val="16,18.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O96">
      <dia:attribute name="obj_pos">
        <dia:point val="17,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.995,15.695;17.005,18.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17,15.7"/>
        <dia:point val="17,18.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O97">
      <dia:attribute name="obj_pos">
        <dia:point val="18,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.995,15.695;18.005,18.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="18,15.7"/>
        <dia:point val="18,18.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O98">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.485,18.185;2.515,20.715"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,18.2"/>
        <dia:point val="2.5,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O99">
      <dia:attribute name="obj_pos">
        <dia:point val="19,20.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,20.685;19.015,20.715"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,20.7"/>
        <dia:point val="2,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O100">
      <dia:attribute name="obj_pos">
        <dia:point val="4,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.985,18.185;4.015,20.715"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4,18.2"/>
        <dia:point val="4,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O101">
      <dia:attribute name="obj_pos">
        <dia:point val="5,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.995,18.195;5.005,20.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5,18.2"/>
        <dia:point val="5,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O102">
      <dia:attribute name="obj_pos">
        <dia:point val="6,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.995,18.195;6.005,20.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6,18.2"/>
        <dia:point val="6,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O103">
      <dia:attribute name="obj_pos">
        <dia:point val="7,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.995,18.195;7.005,20.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="7,18.2"/>
        <dia:point val="7,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O104">
      <dia:attribute name="obj_pos">
        <dia:point val="8,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.995,18.195;8.005,20.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8,18.2"/>
        <dia:point val="8,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O105">
      <dia:attribute name="obj_pos">
        <dia:point val="9,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.995,18.195;9.005,20.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="9,18.2"/>
        <dia:point val="9,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O106">
      <dia:attribute name="obj_pos">
        <dia:point val="10,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.995,18.195;10.005,20.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10,18.2"/>
        <dia:point val="10,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O107">
      <dia:attribute name="obj_pos">
        <dia:point val="11,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.995,18.195;11.005,20.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11,18.2"/>
        <dia:point val="11,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O108">
      <dia:attribute name="obj_pos">
        <dia:point val="12,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.995,18.195;12.005,20.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12,18.2"/>
        <dia:point val="12,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O109">
      <dia:attribute name="obj_pos">
        <dia:point val="13,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.995,18.195;13.005,20.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13,18.2"/>
        <dia:point val="13,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O110">
      <dia:attribute name="obj_pos">
        <dia:point val="14,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.995,18.195;14.005,20.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="14,18.2"/>
        <dia:point val="14,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O111">
      <dia:attribute name="obj_pos">
        <dia:point val="15,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.995,18.195;15.005,20.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15,18.2"/>
        <dia:point val="15,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O112">
      <dia:attribute name="obj_pos">
        <dia:point val="16,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.995,18.195;16.005,20.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="16,18.2"/>
        <dia:point val="16,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O113">
      <dia:attribute name="obj_pos">
        <dia:point val="17,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.995,18.195;17.005,20.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17,18.2"/>
        <dia:point val="17,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O114">
      <dia:attribute name="obj_pos">
        <dia:point val="18,18.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.995,18.195;18.005,20.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="18,18.2"/>
        <dia:point val="18,20.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O115">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,18.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.495,18.695;4.005,18.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,18.7"/>
        <dia:point val="4,18.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O116">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,19.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.495,19.695;4.005,19.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.5,19.7"/>
        <dia:point val="4,19.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O117">
      <dia:attribute name="obj_pos">
        <dia:point val="19,18.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.995,18.695;19.005,18.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,18.7"/>
        <dia:point val="4,18.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O118">
      <dia:attribute name="obj_pos">
        <dia:point val="19,19.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.995,19.695;19.005,19.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,19.7"/>
        <dia:point val="4,19.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O119">
      <dia:attribute name="obj_pos">
        <dia:point val="13.2,12.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.195,12.145;13.205,14.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.2,12.15"/>
        <dia:point val="13.2,14"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O120">
      <dia:attribute name="obj_pos">
        <dia:point val="16.2,12.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.195,12.145;16.205,14.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="16.2,12.15"/>
        <dia:point val="16.2,14"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.0099999997764825821"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O121">
      <dia:attribute name="obj_pos">
        <dia:point val="15.6,26"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.585,25.985;15.615,27.615"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.6,26"/>
        <dia:point val="15.6,27.6"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O122">
      <dia:attribute name="obj_pos">
        <dia:point val="19,26"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.585,25.985;19.015,26.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19,26"/>
        <dia:point val="15.6,26"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
  </dia:layer>
  <dia:layer name="文字" visible="true" active="true">
    <dia:object type="Standard - Text" version="1" id="O123">
      <dia:attribute name="obj_pos">
        <dia:point val="9,4.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9,3.9525;18.3,4.575"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#医療費助成事業　市町村別送付書#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.77611108124256134"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9,4.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="9.8,5.375"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.8,5.075;17.225,5.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#医　　療　　機　　関　　等　　コ　　ー　　ド#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.8,5.375"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="10.05,7.875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.05,7.575;17.4925,7.9175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#保 険 医 療 機 関 等 の 所 在 地 及 び 名 称#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.05,7.875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="7.75,4.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.75,3.9525;8.37,4.575"/>
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
            <dia:real val="0.77611108124256134"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.75,4.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="1.8,2.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.8,2.05;4.8375,2.3925"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（別紙様式第６号）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.8,2.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.15,3.225"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.15,2.7775;2.6575,3.2875"/>
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
            <dia:real val="0.63499997556209564"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.15,3.225"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.9,3.225"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.9,2.7775;3.4075,3.2875"/>
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
            <dia:real val="0.63499997556209564"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.9,3.225"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="3.75,3.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.75,2.7525;4.765,3.2625"/>
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
            <dia:real val="0.63499997556209564"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.75,3.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="12.5,3.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.5,2.7775;13.5575,3.2025"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.5,3.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="15,2.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15,2.5775;16.0575,3.0025"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15,2.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="14.7,3.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.7,3.0275;16.39,3.4525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.7,3.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="17.5,2.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.5,2.5775;18.5575,3.0025"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.5,2.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="17.2,3.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.2,3.0275;18.89,3.4525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.2,3.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.55,7.875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.55,7.575;4.41,7.9175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#市 町 村 名#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.55,7.875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="5.3,7.875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.3,7.575;7.6675,7.9175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#市 町 村 番 号#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.3,7.875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.8,5.375"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.8,5.075;7.1875,5.4175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#種　　別　　コ　　ー　　ド#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.8,5.375"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="8.85,10.825"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.85,10.525;10.71,10.8675"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#電 話 番 号#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,10.825"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.3,12.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.3,12;4.6675,12.3425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#市 町 村 集 計#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.3,12.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.3,13.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.3,13.2;4.675,13.5425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#合          計#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.3,13.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="6.65,11.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.65,11.65;10.3625,11.9925"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入　　　　　　　　　院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.65,11.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="13.65,11.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.65,11.65;17.3625,11.9925"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入　　　　院　　　　外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.65,11.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="5.175,12.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.175,12.3;6.02,12.6425"/>
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
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.175,12.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="6.4,12.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.4,12.3;9.275,12.6425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点 数 （ 金 額 ）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.4,12.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="9.65,12.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.65,12.3;11.51,12.6425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#負 担 金 額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.65,12.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="4.25,15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.25,14.7;16.7375,15.0425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#特　記　事　項　コ　ー　ド　及　び　公　費　負　担　医　療　件　数　再　掲#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.25,15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.075,15.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.075,15.5;2.4125,18.3825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#特
記
事
項
コ
丨
ド#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.075,15.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.075,18.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.075,18.45;2.4125,20.9092"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#公
費
負
担
医
療#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.075,18.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.75,15.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.75,15.55;3.7625,15.8925"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#区　分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.75,15.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.75,16.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.75,16.55;3.7625,16.8925"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入　院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.75,16.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.75,17.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.75,17.55;3.7625,17.8925"/>
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
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.75,17.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="4.3,15.575"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.3,15.275;5.65,15.6175"/>
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
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.3,15.575"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.6,18.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.6,18.3;3.95,18.6425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#法別番号#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.6,18.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.7,19.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.7,19;3.7125,19.3425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入　院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.7,19.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.7,20.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.7,20;3.7125,20.3425"/>
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
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.7,20.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="4.2,16.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.2,15.825;4.8375,16.0825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#02 長#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.2,16.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="5.1,16.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.1,15.825;5.865,16.0825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#16 長2#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.1,16.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="4.25,18.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.25,18.325;4.76,18.5825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１０#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.25,18.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="5.25,18.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.25,18.325;5.76,18.5825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１１#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.25,18.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="6.65,15.575"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.65,15.2664;8.51606,15.6346"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#７０ 歳以上#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.65,15.575"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="10.05,15.575"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.05,15.2664;13.2686,15.6346"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#所 得 区 分（通常）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.05,15.575"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="14.55,15.575"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.55,15.2664;18.7811,15.6346"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#所 得 区 分（多数回該当）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.55,15.575"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="6.05,16.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.05,15.825;6.9425,16.0825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#17 上位#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.05,16.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="7.05,16.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.05,15.825;7.9425,16.0825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#18 一般#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.05,16.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="8.05,16.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.05,15.825;8.9425,16.0825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#19 低所#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.05,16.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="9.05,16.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.05,15.825;9.9425,16.0825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#26 区ア#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.05,16.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="10.05,16.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.05,15.825;10.9425,16.0825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#27 区イ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.05,16.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="11.05,16.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.05,15.825;11.9425,16.0825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#28 区ウ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.05,16.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="12.05,16.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.05,15.825;12.9425,16.0825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#29 区エ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.05,16.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="13.05,16.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.05,15.825;13.9425,16.0825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#30 区オ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.05,16.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="14.05,16.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.05,15.825;14.9425,16.0825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#31 多ア#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.05,16.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="15.05,16.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.05,15.825;15.9425,16.0825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#32 多イ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.05,16.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="16.05,16.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.05,15.825;16.9425,16.0825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#33 多ウ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.05,16.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="17.05,16.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.05,15.825;17.9425,16.0825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#34 多エ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.05,16.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="18.05,16.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.05,15.825;18.9425,16.0825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#35 多オ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.05,16.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="6.25,18.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.25,18.325;6.76,18.5825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１５#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.25,18.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="7.25,18.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.25,18.325;7.76,18.5825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１６#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.25,18.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="8.25,18.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.25,18.325;8.76,18.5825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２０#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.25,18.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="9.25,18.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.25,18.325;9.76,18.5825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２１#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.25,18.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="10.25,18.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.25,18.325;10.76,18.5825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２２#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.25,18.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="11.25,18.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.25,18.325;11.76,18.5825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２４#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,18.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="12.25,18.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.25,18.325;12.76,18.5825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２８#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.25,18.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="13.25,18.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.25,18.325;13.76,18.5825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.25,18.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="14.25,18.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.25,18.325;14.76,18.5825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#３８#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.25,18.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="15.25,18.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.25,18.325;15.76,18.5825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#５１#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.25,18.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="16.25,18.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.25,18.325;16.76,18.5825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#５２#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.25,18.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="17.25,18.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.25,18.325;17.76,18.5825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#５４#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.25,18.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="18.25,18.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.25,18.325;18.76,18.5825"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#７９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.31749998778104782"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.25,18.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="4.45,21.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.45,21.4;16.6,21.7425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#市　　　　町　　　　村　　　　番　　　　号　　　　一　　　　覧　　　　表#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.45,21.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.5,6.875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5,6.4275;3.0075,6.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.63499997556209564"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.5,6.875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="4,6.875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4,6.4275;4.5075,6.9375"/>
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
            <dia:real val="0.63499997556209564"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4,6.875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="5.5,6.875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.5,6.4275;6.0075,6.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#４#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.63499997556209564"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.5,6.875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="7,6.875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7,6.4275;7.5075,6.9375"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#６#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.63499997556209564"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7,6.875"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="5.25,8.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.25,8.5025;5.7575,9.0125"/>
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
            <dia:real val="0.63499997556209564"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.25,8.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.45,5.825"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.45,5.55;3.07,5.8625"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#医科#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.38805554062128067"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.45,5.825"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="3.95,5.825"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.95,5.55;4.57,5.8625"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#歯科#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.38805554062128067"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.95,5.825"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="5.45,5.825"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.45,5.55;6.07,5.8625"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#調剤#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.38805554062128067"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.45,5.825"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="6.95,5.825"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.95,5.55;7.57,5.8625"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#訪問#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.38805554062128067"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.95,5.825"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="12.175,12.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.175,12.3;13.02,12.6425"/>
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
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.175,12.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="13.4,12.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.4,12.3;16.275,12.6425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点 数 （ 金 額 ）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.4,12.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="12.55,27.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.55,27.0125;14.45,27.3975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.55,27.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="15.95,22.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.95,22.2125;17.85,22.5975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.95,22.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="15.95,23.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.95,23.0125;17.85,23.3975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.95,23.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="15.95,24.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.95,23.8125;17.85,24.1975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.95,24.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="15.95,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.95,24.6125;17.85,24.9975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.95,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="15.95,25.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.95,25.4125;17.85,25.7975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.95,25.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="9.15,25.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.15,25.4125;11.05,25.7975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.15,25.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="12.55,22.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.55,22.2125;14.45,22.5975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.55,22.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O210">
      <dia:attribute name="obj_pos">
        <dia:point val="12.55,23.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.55,23.0125;14.45,23.3975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.55,23.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O211">
      <dia:attribute name="obj_pos">
        <dia:point val="12.55,24.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.55,23.8125;14.45,24.1975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.55,24.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O212">
      <dia:attribute name="obj_pos">
        <dia:point val="12.55,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.55,24.6125;14.45,24.9975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.55,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O213">
      <dia:attribute name="obj_pos">
        <dia:point val="12.55,25.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.55,25.4125;14.45,25.7975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.55,25.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O214">
      <dia:attribute name="obj_pos">
        <dia:point val="5.75,24.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.75,23.8125;7.65,24.1975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.75,24.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O215">
      <dia:attribute name="obj_pos">
        <dia:point val="5.75,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.75,24.6125;7.65,24.9975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.75,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O216">
      <dia:attribute name="obj_pos">
        <dia:point val="5.75,25.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.75,25.4125;7.65,25.7975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.75,25.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O217">
      <dia:attribute name="obj_pos">
        <dia:point val="5.75,27.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.75,27.0125;7.65,27.3975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.75,27.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O218">
      <dia:attribute name="obj_pos">
        <dia:point val="9.15,22.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.15,22.2125;11.05,22.5975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.15,22.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O219">
      <dia:attribute name="obj_pos">
        <dia:point val="9.15,23.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.15,23.0125;11.05,23.3975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.15,23.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O220">
      <dia:attribute name="obj_pos">
        <dia:point val="9.15,24.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.15,23.8125;11.05,24.1975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.15,24.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O221">
      <dia:attribute name="obj_pos">
        <dia:point val="9.15,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.15,24.6125;11.05,24.9975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.15,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O222">
      <dia:attribute name="obj_pos">
        <dia:point val="2.35,22.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.35,22.2125;4.25,22.5975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.35,22.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O223">
      <dia:attribute name="obj_pos">
        <dia:point val="2.35,23.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.35,23.0125;4.25,23.3975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.35,23.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O224">
      <dia:attribute name="obj_pos">
        <dia:point val="2.35,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.35,24.6125;4.25,24.9975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.35,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O225">
      <dia:attribute name="obj_pos">
        <dia:point val="2.35,25.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.35,25.4125;4.25,25.7975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.35,25.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O226">
      <dia:attribute name="obj_pos">
        <dia:point val="2.35,26.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.35,26.2125;4.25,26.5975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.35,26.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O227">
      <dia:attribute name="obj_pos">
        <dia:point val="2.35,27.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.35,27.0125;4.25,27.3975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.35,27.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O228">
      <dia:attribute name="obj_pos">
        <dia:point val="5.75,22.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.75,22.2125;7.65,22.5975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.75,22.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O229">
      <dia:attribute name="obj_pos">
        <dia:point val="5.75,23.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.75,23.0125;7.65,23.3975"/>
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
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.75,23.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O230">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45,22.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.45,22.2125;5.21,22.5975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#０１#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.45,22.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O231">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45,23.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.45,23.0125;5.21,23.3975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#０２#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.45,23.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O232">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45,24.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.45,23.8125;5.21,24.1975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#０３#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.45,24.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O233">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.45,24.6125;5.21,24.9975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#０４#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.45,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O234">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45,25.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.45,25.4125;5.21,25.7975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#０５#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.45,25.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O235">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45,26.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.45,26.2125;5.21,26.5975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#０６#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.45,26.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O236">
      <dia:attribute name="obj_pos">
        <dia:point val="4.45,27.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.45,27.0125;5.21,27.3975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#０７#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.45,27.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O237">
      <dia:attribute name="obj_pos">
        <dia:point val="7.85,22.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.85,22.2125;8.61,22.5975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#０８#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.85,22.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O238">
      <dia:attribute name="obj_pos">
        <dia:point val="7.85,23.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.85,23.0125;8.61,23.3975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#０９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.85,23.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O239">
      <dia:attribute name="obj_pos">
        <dia:point val="7.85,24.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.85,23.8125;8.61,24.1975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１０#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.85,24.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O240">
      <dia:attribute name="obj_pos">
        <dia:point val="7.85,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.85,24.6125;8.61,24.9975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１１#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.85,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O241">
      <dia:attribute name="obj_pos">
        <dia:point val="7.85,25.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.85,25.4125;8.61,25.7975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１３#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.85,25.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O242">
      <dia:attribute name="obj_pos">
        <dia:point val="7.85,26.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.85,26.2125;8.61,26.5975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１７#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.85,26.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O243">
      <dia:attribute name="obj_pos">
        <dia:point val="7.85,27.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.85,27.0125;8.61,27.3975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１８#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.85,27.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O244">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,22.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.25,22.2125;12.01,22.5975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１４#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,22.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O245">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,23.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.25,23.0125;12.01,23.3975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１５#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,23.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O246">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,24.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.25,23.8125;12.01,24.1975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１６#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,24.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O247">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.25,24.6125;12.01,24.9975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２１#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O248">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,25.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.25,25.4125;12.01,25.7975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２２#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,25.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O249">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,26.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.25,26.2125;12.01,26.5975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#３０#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,26.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O250">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,27.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.25,27.0125;12.01,27.3975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#３１#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,27.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O251">
      <dia:attribute name="obj_pos">
        <dia:point val="14.65,22.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.65,22.2125;15.41,22.5975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#３６#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.65,22.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O252">
      <dia:attribute name="obj_pos">
        <dia:point val="14.65,23.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.65,23.0125;15.41,23.3975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#４３#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.65,23.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O253">
      <dia:attribute name="obj_pos">
        <dia:point val="14.65,24.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.65,23.8125;15.41,24.1975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#４５#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.65,24.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O254">
      <dia:attribute name="obj_pos">
        <dia:point val="14.65,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.65,24.6125;15.41,24.9975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#４８#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.65,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O255">
      <dia:attribute name="obj_pos">
        <dia:point val="14.65,25.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.65,25.4125;15.41,25.7975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#４９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.65,25.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O256">
      <dia:attribute name="obj_pos">
        <dia:point val="14.65,26.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.65,26.2125;15.41,26.5975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#５０#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.65,26.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O257">
      <dia:attribute name="obj_pos">
        <dia:point val="14.65,27.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.65,27.0125;15.41,27.3975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#５１#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.65,27.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O258">
      <dia:attribute name="obj_pos">
        <dia:point val="18.05,22.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.05,22.2125;18.81,22.5975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#５４#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.05,22.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O259">
      <dia:attribute name="obj_pos">
        <dia:point val="18.05,23.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.05,23.0125;18.81,23.3975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#５５#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.05,23.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O260">
      <dia:attribute name="obj_pos">
        <dia:point val="18.05,24.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.05,23.8125;18.81,24.1975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#５６#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.05,24.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O261">
      <dia:attribute name="obj_pos">
        <dia:point val="18.05,24.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.05,24.6125;18.81,24.9975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#５９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.05,24.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O262">
      <dia:attribute name="obj_pos">
        <dia:point val="18.05,25.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.05,25.4125;18.81,25.7975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#６２#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.47624998167157173"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.05,25.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
      <dia:object type="Standard - Text" version="1" id="O263">
        <dia:attribute name="obj_pos">
          <dia:point val="2.35,24.15"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.35,23.8125;2.73,24.1975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.35,24.15"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O264">
        <dia:attribute name="obj_pos">
          <dia:point val="2.8625,24.15"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.8625,23.8125;3.2425,24.1975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.8625,24.15"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O265">
        <dia:attribute name="obj_pos">
          <dia:point val="3.375,24.15"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3.375,23.8125;3.755,24.1975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="3.375,24.15"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O266">
        <dia:attribute name="obj_pos">
          <dia:point val="3.8875,24.15"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3.8875,23.8125;4.2675,24.1975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="3.8875,24.15"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
      <dia:object type="Standard - Text" version="1" id="O267">
        <dia:attribute name="obj_pos">
          <dia:point val="5.75,26.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="5.75,26.2125;6.13,26.5975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="5.75,26.55"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O268">
        <dia:attribute name="obj_pos">
          <dia:point val="6.25625,26.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.25625,26.2125;6.63625,26.5975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.25625,26.55"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O269">
        <dia:attribute name="obj_pos">
          <dia:point val="6.7625,26.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.7625,26.2125;7.1425,26.5975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.7625,26.55"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O270">
        <dia:attribute name="obj_pos">
          <dia:point val="7.26875,26.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="7.26875,26.2125;7.64875,26.5975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.26875,26.55"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
      <dia:object type="Standard - Text" version="1" id="O271">
        <dia:attribute name="obj_pos">
          <dia:point val="9.15,27.35"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="9.15,27.0125;9.53,27.3975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="9.15,27.35"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O272">
        <dia:attribute name="obj_pos">
          <dia:point val="9.65625,27.35"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="9.65625,27.0125;10.0363,27.3975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="9.65625,27.35"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O273">
        <dia:attribute name="obj_pos">
          <dia:point val="10.6687,27.35"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="10.6687,27.0125;11.0487,27.3975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="10.6687,27.35"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O274">
        <dia:attribute name="obj_pos">
          <dia:point val="10.1625,27.35"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="10.1625,27.0125;10.5425,27.3975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="10.1625,27.35"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
      <dia:object type="Standard - Text" version="1" id="O275">
        <dia:attribute name="obj_pos">
          <dia:point val="9.15,26.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="9.15,26.2125;9.53,26.5975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="9.15,26.55"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O276">
        <dia:attribute name="obj_pos">
          <dia:point val="10.1625,26.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="10.1625,26.2125;10.5425,26.5975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="10.1625,26.55"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O277">
        <dia:attribute name="obj_pos">
          <dia:point val="10.6687,26.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="10.6687,26.2125;11.0487,26.5975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="10.6687,26.55"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O278">
        <dia:attribute name="obj_pos">
          <dia:point val="9.65625,26.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="9.65625,26.2125;10.0363,26.5975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="9.65625,26.55"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
      <dia:object type="Standard - Text" version="1" id="O279">
        <dia:attribute name="obj_pos">
          <dia:point val="12.55,26.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="12.55,26.2125;12.93,26.5975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="12.55,26.55"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O280">
        <dia:attribute name="obj_pos">
          <dia:point val="13.0563,26.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="13.0563,26.2125;13.4363,26.5975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="13.0563,26.55"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O281">
        <dia:attribute name="obj_pos">
          <dia:point val="13.5625,26.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="13.5625,26.2125;13.9425,26.5975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="13.5625,26.55"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O282">
        <dia:attribute name="obj_pos">
          <dia:point val="14.0688,26.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="14.0688,26.2125;14.4488,26.5975"/>
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
              <dia:real val="0.47624998167157173"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="14.0688,26.55"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
    <dia:object type="Standard - Text" version="1" id="O283">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,12.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.55,12.3;18.575,12.6425"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#一部負担金額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42333331704139709"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,12.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
    <dia:object type="Embed - Text" version="1" id="O284">
      <dia:attribute name="obj_pos">
        <dia:point val="11.6,3.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.6,2.7775;12.0225,3.2025"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.6,3.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O285">
      <dia:attribute name="obj_pos">
        <dia:point val="14.1,3.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.1,2.7775;14.5225,3.2025"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.1,3.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O286">
      <dia:attribute name="obj_pos">
        <dia:point val="16.6,3.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6,2.7775;17.0225,3.2025"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.6,3.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O287">
      <dia:attribute name="obj_pos">
        <dia:point val="2.7,4.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.7,3.9525;7.66,4.575"/>
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
            <dia:real val="0.77611108124256134"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.7,4.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O288">
      <dia:attribute name="obj_pos">
        <dia:point val="2.55,6.375"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.55,6.0025;2.9725,6.4275"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.55,6.375"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#TENHYOKBN[0]#</dia:string>
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
        <dia:point val="4.05,6.375"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.05,6.0025;4.4725,6.4275"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.05,6.375"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#TENHYOKBN[1]#</dia:string>
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
        <dia:point val="5.55,6.375"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.55,6.0025;5.9725,6.4275"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.55,6.375"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#TENHYOKBN[2]#</dia:string>
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
        <dia:point val="7.05,6.375"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.05,6.0025;7.4725,6.4275"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.05,6.375"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#TENHYOKBN[3]#</dia:string>
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
        <dia:point val="10.4,6.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.4,5.9525;16.6,6.575"/>
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
            <dia:real val="0.77611108124256134"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4,6.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O293">
      <dia:attribute name="obj_pos">
        <dia:point val="2.25,8.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.25,8.5025;4.7875,9.0125"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえお#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.63499997556209564"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.25,8.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#CITYNAME#</dia:string>
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
        <dia:point val="8.65,8.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.65,8.3375;18.9,9.26153"/>
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
            <dia:real val="0.51152775809168816"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.65,8.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#HOSPADRS#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="25"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O295">
      <dia:attribute name="obj_pos">
        <dia:point val="8.65,9.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.65,9.2375;18.9,10.1615"/>
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
            <dia:real val="0.51152775809168816"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.65,9.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O296">
      <dia:attribute name="obj_pos">
        <dia:point val="11.3,10.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.3,10.4525;15.125,10.9625"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.63499997556209564"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.3,10.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#HOSPTEL#</dia:string>
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
        <dia:point val="4.85,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.2125,16.5275;4.85,16.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.85,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[0].TOKKI-NYUGAI-TBL[0].TOKKIKENSU#</dia:string>
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
        <dia:point val="4.85,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.2125,17.5275;4.85,17.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.85,17.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[0].TOKKI-NYUGAI-TBL[1].TOKKIKENSU#</dia:string>
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
        <dia:point val="5.85,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.2125,16.5275;5.85,16.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.85,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[1].TOKKI-NYUGAI-TBL[0].TOKKIKENSU#</dia:string>
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
        <dia:point val="5.85,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.2125,17.5275;5.85,17.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.85,17.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[1].TOKKI-NYUGAI-TBL[1].TOKKIKENSU#</dia:string>
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
        <dia:point val="6.85,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2125,16.5275;6.85,16.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.85,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[2].TOKKI-NYUGAI-TBL[0].TOKKIKENSU#</dia:string>
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
        <dia:point val="6.85,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2125,17.5275;6.85,17.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.85,17.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[2].TOKKI-NYUGAI-TBL[1].TOKKIKENSU#</dia:string>
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
        <dia:point val="7.85,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2125,16.5275;7.85,16.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.85,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[3].TOKKI-NYUGAI-TBL[0].TOKKIKENSU#</dia:string>
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
        <dia:point val="7.85,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2125,17.5275;7.85,17.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.85,17.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[3].TOKKI-NYUGAI-TBL[1].TOKKIKENSU#</dia:string>
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
        <dia:point val="8.85,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.2125,16.5275;8.85,16.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[4].TOKKI-NYUGAI-TBL[0].TOKKIKENSU#</dia:string>
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
        <dia:point val="8.85,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.2125,17.5275;8.85,17.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,17.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[4].TOKKI-NYUGAI-TBL[1].TOKKIKENSU#</dia:string>
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
        <dia:point val="9.85,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.2125,16.5275;9.85,16.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.85,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[5].TOKKI-NYUGAI-TBL[0].TOKKIKENSU#</dia:string>
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
        <dia:point val="9.85,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.2125,17.5275;9.85,17.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.85,17.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[5].TOKKI-NYUGAI-TBL[1].TOKKIKENSU#</dia:string>
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
        <dia:point val="10.85,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2125,16.5275;10.85,16.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.85,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[6].TOKKI-NYUGAI-TBL[0].TOKKIKENSU#</dia:string>
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
        <dia:point val="10.85,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2125,17.5275;10.85,17.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.85,17.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[6].TOKKI-NYUGAI-TBL[1].TOKKIKENSU#</dia:string>
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
        <dia:point val="11.85,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.2125,16.5275;11.85,16.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.85,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[7].TOKKI-NYUGAI-TBL[0].TOKKIKENSU#</dia:string>
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
        <dia:point val="11.85,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.2125,17.5275;11.85,17.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.85,17.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[7].TOKKI-NYUGAI-TBL[1].TOKKIKENSU#</dia:string>
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
        <dia:point val="12.85,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2125,16.5275;12.85,16.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.85,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[8].TOKKI-NYUGAI-TBL[0].TOKKIKENSU#</dia:string>
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
        <dia:point val="12.85,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2125,17.5275;12.85,17.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.85,17.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[8].TOKKI-NYUGAI-TBL[1].TOKKIKENSU#</dia:string>
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
        <dia:point val="13.85,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.2125,16.5275;13.85,16.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.85,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[9].TOKKI-NYUGAI-TBL[0].TOKKIKENSU#</dia:string>
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
        <dia:point val="13.85,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.2125,17.5275;13.85,17.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.85,17.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[9].TOKKI-NYUGAI-TBL[1].TOKKIKENSU#</dia:string>
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
        <dia:point val="14.85,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.2125,16.5275;14.85,16.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.85,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[10].TOKKI-NYUGAI-TBL[0].TOKKIKENSU#</dia:string>
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
        <dia:point val="14.85,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.2125,17.5275;14.85,17.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.85,17.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[10].TOKKI-NYUGAI-TBL[1].TOKKIKENSU#</dia:string>
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
        <dia:point val="15.85,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2125,16.5275;15.85,16.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.85,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[11].TOKKI-NYUGAI-TBL[0].TOKKIKENSU#</dia:string>
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
        <dia:point val="15.85,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2125,17.5275;15.85,17.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.85,17.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[11].TOKKI-NYUGAI-TBL[1].TOKKIKENSU#</dia:string>
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
        <dia:point val="16.85,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.2125,16.5275;16.85,16.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.85,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[12].TOKKI-NYUGAI-TBL[0].TOKKIKENSU#</dia:string>
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
        <dia:point val="16.85,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.2125,17.5275;16.85,17.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.85,17.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[12].TOKKI-NYUGAI-TBL[1].TOKKIKENSU#</dia:string>
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
        <dia:point val="17.85,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.2125,16.5275;17.85,16.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.85,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[13].TOKKI-NYUGAI-TBL[0].TOKKIKENSU#</dia:string>
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
        <dia:point val="17.85,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.2125,17.5275;17.85,17.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.85,17.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[13].TOKKI-NYUGAI-TBL[1].TOKKIKENSU#</dia:string>
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
        <dia:point val="18.85,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.2125,16.5275;18.85,16.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.85,16.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[14].TOKKI-NYUGAI-TBL[0].TOKKIKENSU#</dia:string>
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
        <dia:point val="18.85,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.2125,17.5275;18.85,17.9525"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.85,17.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#TOKKI-TBL[14].TOKKI-NYUGAI-TBL[1].TOKKIKENSU#</dia:string>
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
        <dia:point val="6.25,8.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.25,8.5025;6.7575,9.0125"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.63499997556209564"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.25,8.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#CITYNUM-TBL.CITYNUM[0]#</dia:string>
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
        <dia:point val="7.25,8.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.25,8.5025;7.7575,9.0125"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.63499997556209564"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.25,8.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#CITYNUM-TBL.CITYNUM[1]#</dia:string>
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
        <dia:point val="6.08848,13.575"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.02598,13.2025;6.08848,13.6275"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.08848,13.575"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU-TBL[0].KENSU#</dia:string>
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
        <dia:point val="9.01348,13.575"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.88848,13.2025;9.01348,13.6275"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.01348,13.575"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU-TBL[0].TENSU#</dia:string>
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
        <dia:point val="11.8135,13.575"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.901,13.2025;11.8135,13.6275"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.8135,13.575"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU-TBL[0].ITBFTN#</dia:string>
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
        <dia:point val="13.126,13.575"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.0635,13.2025;13.126,13.6275"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.126,13.575"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU-TBL[1].KENSU#</dia:string>
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
        <dia:point val="15.951,13.575"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.826,13.2025;15.951,13.6275"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.951,13.575"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU-TBL[1].TENSU#</dia:string>
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
        <dia:point val="18.801,13.575"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.8885,13.2025;18.801,13.6275"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.801,13.575"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIKYU-TBL[1].ITBFTN#</dia:string>
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
        <dia:point val="4.85,19.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.2125,18.995;4.85,19.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.85,19.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[0].KOHI-NYUGAI-TBL[0].KOHIKENSU#</dia:string>
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
        <dia:point val="4.85,20.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.2125,19.995;4.85,20.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.85,20.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[0].KOHI-NYUGAI-TBL[1].KOHIKENSU#</dia:string>
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
        <dia:point val="5.85,19.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.2125,18.995;5.85,19.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.85,19.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[1].KOHI-NYUGAI-TBL[0].KOHIKENSU#</dia:string>
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
        <dia:point val="5.85,20.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.2125,19.995;5.85,20.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.85,20.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[1].KOHI-NYUGAI-TBL[1].KOHIKENSU#</dia:string>
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
        <dia:point val="6.85,19.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2125,18.995;6.85,19.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.85,19.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[2].KOHI-NYUGAI-TBL[0].KOHIKENSU#</dia:string>
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
        <dia:point val="6.85,20.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2125,19.995;6.85,20.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.85,20.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[2].KOHI-NYUGAI-TBL[1].KOHIKENSU#</dia:string>
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
        <dia:point val="7.85,19.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2125,18.995;7.85,19.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.85,19.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[3].KOHI-NYUGAI-TBL[0].KOHIKENSU#</dia:string>
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
        <dia:point val="7.85,20.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2125,19.995;7.85,20.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.85,20.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[3].KOHI-NYUGAI-TBL[1].KOHIKENSU#</dia:string>
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
        <dia:point val="8.85,19.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.2125,18.995;8.85,19.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,19.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[4].KOHI-NYUGAI-TBL[0].KOHIKENSU#</dia:string>
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
        <dia:point val="8.85,20.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.2125,19.995;8.85,20.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,20.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[4].KOHI-NYUGAI-TBL[1].KOHIKENSU#</dia:string>
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
        <dia:point val="9.85,19.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.2125,18.995;9.85,19.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.85,19.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[5].KOHI-NYUGAI-TBL[0].KOHIKENSU#</dia:string>
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
        <dia:point val="9.85,20.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.2125,19.995;9.85,20.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.85,20.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[5].KOHI-NYUGAI-TBL[1].KOHIKENSU#</dia:string>
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
        <dia:point val="10.85,19.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2125,18.995;10.85,19.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.85,19.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[6].KOHI-NYUGAI-TBL[0].KOHIKENSU#</dia:string>
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
        <dia:point val="10.85,20.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.2125,19.995;10.85,20.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.85,20.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[6].KOHI-NYUGAI-TBL[1].KOHIKENSU#</dia:string>
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
        <dia:point val="11.85,19.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.2125,18.995;11.85,19.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.85,19.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[7].KOHI-NYUGAI-TBL[0].KOHIKENSU#</dia:string>
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
        <dia:point val="11.85,20.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.2125,19.995;11.85,20.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.85,20.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[7].KOHI-NYUGAI-TBL[1].KOHIKENSU#</dia:string>
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
        <dia:point val="12.85,19.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2125,18.995;12.85,19.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.85,19.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[8].KOHI-NYUGAI-TBL[0].KOHIKENSU#</dia:string>
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
        <dia:point val="12.85,20.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2125,19.995;12.85,20.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.85,20.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[8].KOHI-NYUGAI-TBL[1].KOHIKENSU#</dia:string>
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
        <dia:point val="13.85,19.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.2125,18.995;13.85,19.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.85,19.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[9].KOHI-NYUGAI-TBL[0].KOHIKENSU#</dia:string>
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
        <dia:point val="13.85,20.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.2125,19.995;13.85,20.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.85,20.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[9].KOHI-NYUGAI-TBL[1].KOHIKENSU#</dia:string>
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
        <dia:point val="14.85,19.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.2125,18.995;14.85,19.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.85,19.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[10].KOHI-NYUGAI-TBL[0].KOHIKENSU#</dia:string>
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
        <dia:point val="14.85,20.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.2125,19.995;14.85,20.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.85,20.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[10].KOHI-NYUGAI-TBL[1].KOHIKENSU#</dia:string>
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
        <dia:point val="15.85,19.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2125,18.995;15.85,19.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.85,19.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[11].KOHI-NYUGAI-TBL[0].KOHIKENSU#</dia:string>
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
        <dia:point val="15.85,20.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2125,19.995;15.85,20.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.85,20.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[11].KOHI-NYUGAI-TBL[1].KOHIKENSU#</dia:string>
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
        <dia:point val="16.85,19.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.2125,18.995;16.85,19.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.85,19.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[12].KOHI-NYUGAI-TBL[0].KOHIKENSU#</dia:string>
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
        <dia:point val="16.85,20.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.2125,19.995;16.85,20.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.85,20.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[12].KOHI-NYUGAI-TBL[1].KOHIKENSU#</dia:string>
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
        <dia:point val="17.85,19.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.2125,18.995;17.85,19.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.85,19.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[13].KOHI-NYUGAI-TBL[0].KOHIKENSU#</dia:string>
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
        <dia:point val="17.85,20.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.2125,19.995;17.85,20.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.85,20.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[13].KOHI-NYUGAI-TBL[1].KOHIKENSU#</dia:string>
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
        <dia:point val="18.85,19.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.2125,18.995;18.85,19.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.85,19.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[14].KOHI-NYUGAI-TBL[0].KOHIKENSU#</dia:string>
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
        <dia:point val="18.85,20.3675"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.2125,19.995;18.85,20.42"/>
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
            <dia:real val="0.52916664630174637"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.85,20.3675"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOHI-TBL[14].KOHI-NYUGAI-TBL[1].KOHIKENSU#</dia:string>
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
