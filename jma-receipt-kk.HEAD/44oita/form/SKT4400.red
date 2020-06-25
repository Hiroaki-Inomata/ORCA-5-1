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
        <dia:embed object="string" length="14"/>
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
    <dia:element name="KAISETU" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="60"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="TEL" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="15"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="KENKBN" occurs="2">
      <dia:appinfo>
        <dia:embed object="string" length="2"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="TOKUBETU" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="10"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="SEIKYU-TBL" occurs="2">
      <dia:element name="NYUGAI-TBL" occurs="2">
        <dia:element name="KENSU" occurs="1">
          <dia:appinfo>
            <dia:embed object="string" length="6"/>
          </dia:appinfo>
        </dia:element>
        <dia:element name="TENSU" occurs="1">
          <dia:appinfo>
            <dia:embed object="string" length="10"/>
          </dia:appinfo>
        </dia:element>
        <dia:element name="HEIKIN" occurs="1">
          <dia:appinfo>
            <dia:embed object="string" length="7"/>
          </dia:appinfo>
        </dia:element>
      </dia:element>
      <dia:element name="GKENSU" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="6"/>
        </dia:appinfo>
      </dia:element>
    </dia:element>
    <dia:element name="SAI-KENSU" occurs="2">
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
  <dia:layer name="背景" visible="true">
    <dia:object type="Standard - Box" version="0" id="O0">
      <dia:attribute name="obj_pos">
        <dia:point val="2,13.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,13.185;19.215,21.415"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="2,13.2"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="17.2"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="8.2"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.03"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O1">
      <dia:attribute name="obj_pos">
        <dia:point val="2,22.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,22.085;19.215,26.615"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="2,22.1"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="17.2"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="4.5"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.03"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O2">
      <dia:attribute name="obj_pos">
        <dia:point val="6.5,13.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.485,13.185;6.515,21.415"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6.5,13.2"/>
        <dia:point val="6.5,21.4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O3">
      <dia:attribute name="obj_pos">
        <dia:point val="6.5,22.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.485,22.085;6.515,26.615"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6.5,22.1"/>
        <dia:point val="6.5,26.6"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O4">
      <dia:attribute name="obj_pos">
        <dia:point val="2,14.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,14.785;19.215,14.815"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,14.8"/>
        <dia:point val="19.2,14.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O5">
      <dia:attribute name="obj_pos">
        <dia:point val="4.5,14.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.485,14.785;4.515,21.415"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.5,14.8"/>
        <dia:point val="4.5,21.4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O6">
      <dia:attribute name="obj_pos">
        <dia:point val="4.5,15.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.485,15.885;19.215,15.915"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.5,15.9"/>
        <dia:point val="19.2,15.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O7">
      <dia:attribute name="obj_pos">
        <dia:point val="4.5,17"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.485,16.985;14.915,17.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.5,17"/>
        <dia:point val="14.9,17"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O8">
      <dia:attribute name="obj_pos">
        <dia:point val="2,18.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,18.085;19.215,18.115"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2,18.1"/>
        <dia:point val="19.2,18.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O9">
      <dia:attribute name="obj_pos">
        <dia:point val="4.5,19.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.485,19.185;19.215,19.215"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.5,19.2"/>
        <dia:point val="19.2,19.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O10">
      <dia:attribute name="obj_pos">
        <dia:point val="4.5,20.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.485,20.285;14.915,20.315"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.5,20.3"/>
        <dia:point val="14.9,20.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O11">
      <dia:attribute name="obj_pos">
        <dia:point val="3,22.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.985,22.085;3.015,26.615"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3,22.1"/>
        <dia:point val="3,26.6"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O12">
      <dia:attribute name="obj_pos">
        <dia:point val="2,11.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,11.885;6.515,12.715"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="2,11.9"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="4.5"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="0.8"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.03"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O13">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7,14"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.68,13.98;10.72,21.42"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.7,14"/>
        <dia:point val="10.7,21.4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.04"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O14">
      <dia:attribute name="obj_pos">
        <dia:point val="19.2,14"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.485,13.985;19.215,14.015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.2,14"/>
        <dia:point val="6.5,14"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O15">
      <dia:attribute name="obj_pos">
        <dia:point val="14.9,14"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.88,13.98;14.92,21.42"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="14.9,14"/>
        <dia:point val="14.9,21.4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.04"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O16">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7,22.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.685,22.085;10.715,26.615"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.7,22.1"/>
        <dia:point val="10.7,26.6"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O17">
      <dia:attribute name="obj_pos">
        <dia:point val="14.9,22.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.885,22.085;14.915,26.615"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="14.9,22.1"/>
        <dia:point val="14.9,26.6"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O18">
      <dia:attribute name="obj_pos">
        <dia:point val="3,23.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.985,23.185;19.215,23.215"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3,23.2"/>
        <dia:point val="19.2,23.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O19">
      <dia:attribute name="obj_pos">
        <dia:point val="3,24.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.985,24.885;19.215,24.915"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3,24.9"/>
        <dia:point val="19.2,24.9"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O20">
      <dia:attribute name="obj_pos">
        <dia:point val="10.2,11.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.185,11.885;19.215,12.715"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="10.2,11.9"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="9"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="0.8"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.03"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O21">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4,11.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.385,11.885;13.415,12.715"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.4,11.9"/>
        <dia:point val="13.4,12.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O22">
      <dia:attribute name="obj_pos">
        <dia:point val="2,6.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,6.185;6.515,8.315"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="2,6.2"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="4.5"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="2.1"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.03"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O23">
      <dia:attribute name="obj_pos">
        <dia:point val="6.5,7.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,7.085;6.515,7.115"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6.5,7.1"/>
        <dia:point val="2,7.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O24">
      <dia:attribute name="obj_pos">
        <dia:point val="10.2,6.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.185,6.185;19.215,8.315"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="10.2,6.2"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="9"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="2.1"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.03"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O25">
      <dia:attribute name="obj_pos">
        <dia:point val="19.2,7.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.185,7.085;19.215,7.115"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.2,7.1"/>
        <dia:point val="10.2,7.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O26">
      <dia:attribute name="obj_pos">
        <dia:point val="3.5,6.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.485,6.185;3.515,8.315"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.5,6.2"/>
        <dia:point val="3.5,8.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O27">
      <dia:attribute name="obj_pos">
        <dia:point val="5,6.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.985,6.185;5.015,8.315"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5,6.2"/>
        <dia:point val="5,8.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O28">
      <dia:attribute name="obj_pos">
        <dia:point val="2,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.985,5.185;6.015,5.815"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="2,5.2"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="4"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="0.6"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.03"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O29">
      <dia:attribute name="obj_pos">
        <dia:point val="4,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.985,5.185;4.015,5.815"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4,5.2"/>
        <dia:point val="4,5.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O28" connection="1"/>
        <dia:connection handle="1" to="O28" connection="6"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O30">
      <dia:attribute name="obj_pos">
        <dia:point val="11.2,6.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.185,6.185;11.215,8.315"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.2,6.2"/>
        <dia:point val="11.2,8.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O31">
      <dia:attribute name="obj_pos">
        <dia:point val="13.3,6.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.285,6.185;13.315,8.315"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.3,6.2"/>
        <dia:point val="13.3,8.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O32">
      <dia:attribute name="obj_pos">
        <dia:point val="19.2,19.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.885,19.185;19.215,21.415"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.2,19.2"/>
        <dia:point val="14.9,21.4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O33">
      <dia:attribute name="obj_pos">
        <dia:point val="19.2,15.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.885,15.885;19.215,18.115"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.2,15.9"/>
        <dia:point val="14.9,18.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.03"/>
      </dia:attribute>
    </dia:object>
  </dia:layer>
  <dia:layer name="文字" visible="true">
    <dia:object type="Standard - Text" version="0" id="O34">
      <dia:attribute name="obj_pos">
        <dia:point val="4.7,25.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.7,25.6713;5.0535,26.0213"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#万#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.7,25.95"/>
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
    <dia:object type="Standard - Text" version="0" id="O35">
      <dia:attribute name="obj_pos">
        <dia:point val="5.35,25.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.35,25.6713;5.7035,26.0213"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#以#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.35,25.95"/>
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
    <dia:object type="Standard - Text" version="0" id="O36">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4,3.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.4,3.34259;16.056,4.04259"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#分診療報酬総括票#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.875000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4,3.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.4242"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.3535"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O37">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,5.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,5.33148;2.504,5.73148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#県#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.1,5.65"/>
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
    <dia:object type="Standard - Text" version="0" id="O38">
      <dia:attribute name="obj_pos">
        <dia:point val="2.8,5.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.8,5.33148;3.204,5.73148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#内#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.8,5.65"/>
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
    <dia:object type="Standard - Text" version="0" id="O39">
      <dia:attribute name="obj_pos">
        <dia:point val="3.5,5.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.5,5.33148;3.904,5.73148"/>
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
            <dia:point val="3.5,5.65"/>
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
    <dia:object type="Standard - Text" version="0" id="O40">
      <dia:attribute name="obj_pos">
        <dia:point val="4.1,5.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.1,5.33148;4.504,5.73148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#県#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.1,5.65"/>
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
    <dia:object type="Standard - Text" version="0" id="O41">
      <dia:attribute name="obj_pos">
        <dia:point val="4.8,5.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8,5.33148;5.204,5.73148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.8,5.65"/>
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
    <dia:object type="Standard - Text" version="0" id="O42">
      <dia:attribute name="obj_pos">
        <dia:point val="5.5,5.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.5,5.33148;5.904,5.73148"/>
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
            <dia:point val="5.5,5.65"/>
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
    <dia:object type="Standard - Text" version="0" id="O43">
      <dia:attribute name="obj_pos">
        <dia:point val="2.15,6.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.15,6.51111;3.362,6.81111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#事業区分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.15,6.75"/>
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
    <dia:object type="Standard - Text" version="0" id="O44">
      <dia:attribute name="obj_pos">
        <dia:point val="3.65,6.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.65,6.51111;4.862,6.81111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#処理区分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.65,6.75"/>
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
    <dia:object type="Standard - Text" version="0" id="O45">
      <dia:attribute name="obj_pos">
        <dia:point val="5.35,6.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.35,6.51111;6.259,6.81111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#係番号#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.35,6.75"/>
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
    <dia:object type="Standard - Text" version="0" id="O46">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,6.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.3,6.48148;11.108,6.88148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#県番#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,6.8"/>
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
    <dia:object type="Standard - Text" version="0" id="O47">
      <dia:attribute name="obj_pos">
        <dia:point val="13.65,6.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.65,6.48148;18.902,6.88148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#医　療　機　関　コ　ー　ド#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.65,6.8"/>
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
    <dia:object type="Standard - Text" version="0" id="O48">
      <dia:attribute name="obj_pos">
        <dia:point val="2.5,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.5,7.50185;3.005,8.00185"/>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.5,7.9"/>
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
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O49">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4,6.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.4,6.48148;11.804,6.88148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#医#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4,6.8"/>
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
    <dia:object type="Standard - Text" version="0" id="O50">
      <dia:attribute name="obj_pos">
        <dia:point val="12.1,6.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.1,6.48148;12.504,6.88148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#歯#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.1,6.8"/>
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
    <dia:object type="Standard - Text" version="0" id="O51">
      <dia:attribute name="obj_pos">
        <dia:point val="12.8,6.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.8,6.48148;13.204,6.88148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#調#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.8,6.8"/>
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
    <dia:object type="Standard - Text" version="0" id="O52">
      <dia:attribute name="obj_pos">
        <dia:point val="10.25,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.25,7.50185;11.26,8.00185"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#４４#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.25,7.9"/>
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
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O53">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.4,7.50185;11.905,8.00185"/>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4,7.9"/>
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
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O54">
      <dia:attribute name="obj_pos">
        <dia:point val="12.1,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.1,7.50185;12.605,8.00185"/>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.1,7.9"/>
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
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O55">
      <dia:attribute name="obj_pos">
        <dia:point val="12.8,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.8,7.50185;13.305,8.00185"/>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.8,7.9"/>
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
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O56">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.85,8.68148;11.678,9.08148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#保険医療機関の#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,9"/>
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
    <dia:object type="Standard - Text" version="0" id="O57">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,9.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.85,9.23148;11.678,9.63148"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#所在地及び名称#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,9.55"/>
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
    <dia:object type="Standard - Text" version="0" id="O58">
      <dia:attribute name="obj_pos">
        <dia:point val="9.4,11.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.4,10.9315;11.42,11.3315"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#開設者氏名#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4,11.25"/>
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
    <dia:object type="Standard - Text" version="0" id="O59">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4,12.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.4,12.1315;13.228,12.5315"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#電　話　番　号#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4,12.45"/>
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
    <dia:object type="Standard - Text" version="0" id="O60">
      <dia:attribute name="obj_pos">
        <dia:point val="2.45,12.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.45,12.1315;6.086,12.5315"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#事　審　縦　減　分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.45,12.45"/>
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
    <dia:object type="Standard - Text" version="0" id="O61">
      <dia:attribute name="obj_pos">
        <dia:point val="3.1,14.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.1,13.7815;5.524,14.1815"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#区　　　　分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.1,14.1"/>
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
    <dia:object type="Standard - Text" version="0" id="O62">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,16.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,16.1213;4.321,16.4713"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#国民健康保険#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,16.4"/>
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
    <dia:object type="Standard - Text" version="0" id="O63">
      <dia:attribute name="obj_pos">
        <dia:point val="2.05,16.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.05,16.6213;4.5245,16.9713"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（一般・退職）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.05,16.9"/>
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
    <dia:object type="Standard - Text" version="0" id="O64">
      <dia:attribute name="obj_pos">
        <dia:point val="2.1,19.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.1,19.6213;2.4535,19.9713"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#後#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.1,19.9"/>
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
    <dia:object type="Standard - Text" version="0" id="O65">
      <dia:attribute name="obj_pos">
        <dia:point val="2.75,19.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.75,19.6213;3.1035,19.9713"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#期#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.75,19.9"/>
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
        <dia:point val="3.4,19.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.4,19.6213;3.7535,19.9713"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#高#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.4,19.9"/>
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
    <dia:object type="Standard - Text" version="0" id="O67">
      <dia:attribute name="obj_pos">
        <dia:point val="4.05,19.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.05,19.6213;4.4035,19.9713"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#齢#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.05,19.9"/>
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
    <dia:object type="Standard - Text" version="0" id="O68">
      <dia:attribute name="obj_pos">
        <dia:point val="4.85,15.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.85,15.2213;6.264,15.5713"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件　　数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.85,15.5"/>
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
    <dia:object type="Standard - Text" version="0" id="O69">
      <dia:attribute name="obj_pos">
        <dia:point val="4.85,16.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.85,16.3213;6.264,16.6713"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点　　数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.85,16.6"/>
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
    <dia:object type="Standard - Text" version="0" id="O70">
      <dia:attribute name="obj_pos">
        <dia:point val="4.6,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.6,17.4213;6.3675,17.7713"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１件当点数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.6,17.7"/>
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
    <dia:object type="Standard - Text" version="0" id="O71">
      <dia:attribute name="obj_pos">
        <dia:point val="4.85,18.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.85,18.5213;6.264,18.8713"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件　　数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.85,18.8"/>
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
    <dia:object type="Standard - Text" version="0" id="O72">
      <dia:attribute name="obj_pos">
        <dia:point val="4.85,19.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.85,19.6213;6.264,19.9713"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点　　数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.85,19.9"/>
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
    <dia:object type="Standard - Text" version="0" id="O73">
      <dia:attribute name="obj_pos">
        <dia:point val="4.6,21"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.6,20.7213;6.3675,21.0713"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１件当点数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.6,21"/>
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
    <dia:object type="Standard - Text" version="0" id="O74">
      <dia:attribute name="obj_pos">
        <dia:point val="7.55,14.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.55,14.2713;9.671,14.6213"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入　　　　院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.55,14.55"/>
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
    <dia:object type="Standard - Text" version="0" id="O75">
      <dia:attribute name="obj_pos">
        <dia:point val="11.6,14.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.6,14.2713;14.0745,14.6213"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入　　院　　外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.6,14.55"/>
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
    <dia:object type="Standard - Text" version="0" id="O76">
      <dia:attribute name="obj_pos">
        <dia:point val="16,14.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16,14.2713;18.121,14.6213"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#合　　　　計#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16,14.55"/>
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
    <dia:object type="Standard - Text" version="0" id="O77">
      <dia:attribute name="obj_pos">
        <dia:point val="12.8,13.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.79525,13.4713;15.8048,13.8213"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#療　　　養　　　の　　　給　　　付#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.8,13.75"/>
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
    <dia:object type="Standard - Text" version="0" id="O78">
      <dia:attribute name="obj_pos">
        <dia:point val="2.3,23.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.3,23.0713;2.6535,25.5213"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#再





掲#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.3,23.35"/>
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
    <dia:object type="Standard - Text" version="0" id="O79">
      <dia:attribute name="obj_pos">
        <dia:point val="3.6,22.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.6,22.4815;6.024,22.8815"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#区　　　　分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.6,22.8"/>
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
    <dia:object type="Standard - Text" version="0" id="O80">
      <dia:attribute name="obj_pos">
        <dia:point val="3.25,23.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.25,23.6213;4.664,23.9713"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#８万点〜#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.25,23.9"/>
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
    <dia:object type="Standard - Text" version="0" id="O81">
      <dia:attribute name="obj_pos">
        <dia:point val="4.15,24.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.15,24.1213;6.271,24.4713"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１５万点未満#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.15,24.4"/>
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
    <dia:object type="Standard - Text" version="0" id="O82">
      <dia:attribute name="obj_pos">
        <dia:point val="3.2,25.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.2,25.6713;3.907,26.0213"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.2,25.95"/>
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
    <dia:object type="Standard - Text" version="0" id="O83">
      <dia:attribute name="obj_pos">
        <dia:point val="7.4,22.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.4,22.5213;9.8745,22.8713"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#提　出　件　数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.4,22.8"/>
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
    <dia:object type="Standard - Text" version="0" id="O84">
      <dia:attribute name="obj_pos">
        <dia:point val="11.75,22.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.75,22.5213;13.871,22.8713"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#連合会記入欄#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.75,22.8"/>
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
    <dia:object type="Standard - Text" version="0" id="O85">
      <dia:attribute name="obj_pos">
        <dia:point val="15.95,22.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.95,22.5213;18.4245,22.8713"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#備　　　　　考#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.95,22.8"/>
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
    <dia:object type="Standard - Text" version="0" id="O86">
      <dia:attribute name="obj_pos">
        <dia:point val="4.05,25.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.05,25.6713;4.4035,26.0213"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.05,25.95"/>
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
    <dia:object type="Standard - Text" version="0" id="O87">
      <dia:attribute name="obj_pos">
        <dia:point val="6,25.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6,25.6713;6.3535,26.0213"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#上#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6,25.95"/>
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
    <dia:object type="Standard - Text" version="0" id="O88">
      <dia:attribute name="obj_pos">
        <dia:point val="11.35,7.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.35,7.47222;11.956,8.07222"/>
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
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.35,7.95"/>
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
    </dia:object>
  </dia:layer>
  <dia:layer name="差し込み" visible="true">
    <dia:object type="Embed - Text" version="0" id="O89">
      <dia:attribute name="obj_pos">
        <dia:point val="4.8,3.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8,3.34259;10.456,4.04259"/>
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
            <dia:real val="0.875000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.8,3.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.4242"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.3535"/>
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
    <dia:object type="Embed - Text" version="0" id="O90">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,7.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8,7.34259;18.749,8.04259"/>
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
            <dia:string>#１２３４５６７#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.875000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,7.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.4242"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.3535"/>
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
    <dia:object type="Embed - Text" version="0" id="O91">
      <dia:attribute name="obj_pos">
        <dia:point val="12,9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12,8.76111;19.575,9.36111"/>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12,9"/>
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
        <dia:string>#ADRS#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="100"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="25"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O92">
      <dia:attribute name="obj_pos">
        <dia:point val="12,9.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12,9.61111;18.06,10.2111"/>
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
            <dia:string>#いいいいいいいいいいいいいいいいいいいい
いいいいいいいいいいいいいいいいいいいい#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12,9.85"/>
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
        <dia:point val="12,11.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12,10.9213;19.07,11.6213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KAISETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="40"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#うううううううううううううううううううう
うううううううううう#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12,11.2"/>
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
      <dia:attribute name="embed_id">
        <dia:string>#KAISETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="60"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="20"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O94">
      <dia:attribute name="obj_pos">
        <dia:point val="13.85,12.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.85,12.0519;18.395,12.5519"/>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.85,12.45"/>
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
        <dia:string>#TEL#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="15"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O95">
      <dia:attribute name="obj_pos">
        <dia:point val="2.75,5.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.75,5.30185;3.255,5.80185"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KENKBN[0]#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.75,5.7"/>
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
        <dia:string>#KENKBN[0]#</dia:string>
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
        <dia:point val="4.75,5.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.75,5.30185;5.255,5.80185"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KENKBN[1]#</dia:string>
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
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.75,5.7"/>
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
        <dia:string>#KENKBN[1]#</dia:string>
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
        <dia:point val="10.075,15.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.8934,15.1222;10.075,15.7222"/>
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
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.075,15.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O98">
      <dia:attribute name="obj_pos">
        <dia:point val="10.075,16.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.439,16.2222;10.075,16.8222"/>
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
            <dia:string>#99,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.075,16.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[0].TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O99">
      <dia:attribute name="obj_pos">
        <dia:point val="10.075,17.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.5298,17.3222;10.075,17.9222"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[0].HEIKIN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.075,17.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[0].HEIKIN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O100">
      <dia:attribute name="obj_pos">
        <dia:point val="14.425,15.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2434,15.1222;14.425,15.7222"/>
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
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.425,15.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O101">
      <dia:attribute name="obj_pos">
        <dia:point val="14.425,16.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.789,16.2222;14.425,16.8222"/>
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
            <dia:string>#99,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.425,16.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[1].TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O102">
      <dia:attribute name="obj_pos">
        <dia:point val="14.425,17.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.8798,17.3222;14.425,17.9222"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[1].HEIKIN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.425,17.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SEIKYU-TBL[0].NYUGAI-TBL[1].HEIKIN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O103">
      <dia:attribute name="obj_pos">
        <dia:point val="18.675,15.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.4934,15.1222;18.675,15.7222"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[0].GKENSU#</dia:string>
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
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.675,15.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SEIKYU-TBL[0].GKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O104">
      <dia:attribute name="obj_pos">
        <dia:point val="10.075,18.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.8934,18.4222;10.075,19.0222"/>
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
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.075,18.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O105">
      <dia:attribute name="obj_pos">
        <dia:point val="10.075,20"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.439,19.5222;10.075,20.1222"/>
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
            <dia:string>#99,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.075,20"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[0].TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O106">
      <dia:attribute name="obj_pos">
        <dia:point val="10.075,21.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.5298,20.6222;10.075,21.2222"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[0].HEIKIN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.075,21.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[0].HEIKIN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O107">
      <dia:attribute name="obj_pos">
        <dia:point val="14.425,18.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.2434,18.4222;14.425,19.0222"/>
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
            <dia:string>#99,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.425,18.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O108">
      <dia:attribute name="obj_pos">
        <dia:point val="14.425,20"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.789,19.5222;14.425,20.1222"/>
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
            <dia:string>#99,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.425,20"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[1].TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O109">
      <dia:attribute name="obj_pos">
        <dia:point val="14.425,21.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.8798,20.6222;14.425,21.2222"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[1].HEIKIN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.425,21.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SEIKYU-TBL[1].NYUGAI-TBL[1].HEIKIN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O110">
      <dia:attribute name="obj_pos">
        <dia:point val="18.675,18.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.4934,18.4222;18.675,19.0222"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIKYU-TBL[1].GKENSU#</dia:string>
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
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.675,18.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SEIKYU-TBL[1].GKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O111">
      <dia:attribute name="obj_pos">
        <dia:point val="10.075,24.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.8934,23.8222;10.075,24.4222"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SAI-KENSU[0]#</dia:string>
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
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.075,24.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SAI-KENSU[0]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O112">
      <dia:attribute name="obj_pos">
        <dia:point val="10.075,26.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.8934,25.6222;10.075,26.2222"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SAI-KENSU[1]#</dia:string>
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
            <dia:real val="0.750000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.075,26.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SAI-KENSU[1]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O113">
      <dia:attribute name="obj_pos">
        <dia:point val="9.47329,2.40102"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.47329,2.0825;11.4933,2.4825"/>
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
            <dia:point val="9.47329,2.40102"/>
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
