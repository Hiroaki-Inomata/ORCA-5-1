<?xml version="1.0" encoding="UTF-8"?>
<dia:diagram xmlns:dia="http://www.lysator.liu.se/~alla/dia/">
  <dia:dictionarydata>
    <element name="HOSPCD-G" occurs="7">
      <element name="HOSPCD" occurs="1">
        <appinfo>
          <embed object="text" length="2"/>
        </appinfo>
      </element>
    </element>
    <element name="SEIYMD" occurs="1">
      <appinfo>
        <embed object="text" length="22"/>
      </appinfo>
    </element>
    <element name="CITYNAME" occurs="2">
      <appinfo>
        <embed object="text" length="20"/>
      </appinfo>
    </element>
    <element name="POST" occurs="1">
      <appinfo>
        <embed object="text" length="18"/>
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
    <element name="KAISETU" occurs="1">
      <appinfo>
        <embed object="text" length="40"/>
      </appinfo>
    </element>
    <element name="TEL" occurs="1">
      <appinfo>
        <embed object="text" length="24"/>
      </appinfo>
    </element>
    <element name="CITYCD" occurs="3">
      <appinfo>
        <embed object="text" length="1"/>
      </appinfo>
    </element>
    <element name="SEIYM" occurs="1">
      <appinfo>
        <embed object="text" length="16"/>
      </appinfo>
    </element>
    <element name="KENSU-TBL" occurs="6">
      <element name="KENSU" occurs="1">
        <appinfo>
          <embed object="text" length="5"/>
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
        <dia:point val="6.45,2.45034"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.435,2.43534;18.4657,5.015"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="6.45,2.45034"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="12.0007"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="2.5496599999999998"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.029999999999999999"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O1">
      <dia:attribute name="obj_pos">
        <dia:point val="2.4,5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.385,4.985;18.465,24.565"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="2.4,5"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="16.050000000000001"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="19.550000000000001"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.029999999999999999"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O2">
      <dia:attribute name="obj_pos">
        <dia:point val="6.45,3.72517"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.435,3.71017;18.4657,3.74017"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6.45,3.72517"/>
        <dia:point val="18.4507,3.72517"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999999999999"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O0" connection="3"/>
        <dia:connection handle="1" to="O0" connection="4"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O3">
      <dia:attribute name="obj_pos">
        <dia:point val="6.45,3.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.445,3.245;18.4557,3.255"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6.45,3.25"/>
        <dia:point val="18.4507,3.25"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O4">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,3.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.845,3.245;8.855,5.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.85,3.25"/>
        <dia:point val="8.85,5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O5">
      <dia:attribute name="obj_pos">
        <dia:point val="12.45,3.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.445,3.245;12.455,5.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.45,3.25"/>
        <dia:point val="12.45,5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O6">
      <dia:attribute name="obj_pos">
        <dia:point val="17.25,3.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.245,3.245;17.2553,5.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.25,3.25"/>
        <dia:point val="17.2503,5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O7">
      <dia:attribute name="obj_pos">
        <dia:point val="10.05,3.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.045,3.245;10.055,5.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="10.05,3.25"/>
        <dia:point val="10.05,5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O8">
      <dia:attribute name="obj_pos">
        <dia:point val="7.65,3.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.645,3.745;7.655,5.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="7.65,3.75"/>
        <dia:point val="7.65,5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O9">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,3.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.245,3.745;11.255,5.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.25,3.75"/>
        <dia:point val="11.25,5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O10">
      <dia:attribute name="obj_pos">
        <dia:point val="13.65,3.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.645,3.745;13.655,5.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.65,3.75"/>
        <dia:point val="13.65,5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O11">
      <dia:attribute name="obj_pos">
        <dia:point val="14.85,3.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.845,3.745;14.855,5.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="14.85,3.75"/>
        <dia:point val="14.85,5"/>
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
        <dia:point val="16.05,3.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.045,3.745;16.055,5.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="16.05,3.75"/>
        <dia:point val="16.05,5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O13">
      <dia:attribute name="obj_pos">
        <dia:point val="4,15.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.985,15.085;15.615,24.165"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="4,15.1"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="11.6"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="9.0500000000000025"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.029999999999999999"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O14">
      <dia:attribute name="obj_pos">
        <dia:point val="4,16.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.985,16.085;15.615,16.115"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4,16.1"/>
        <dia:point val="15.6,16.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999999999999"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O15">
      <dia:attribute name="obj_pos">
        <dia:point val="4,17.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.985,17.135;15.615,17.165"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4,17.15"/>
        <dia:point val="15.6,17.15"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999999999999"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O16">
      <dia:attribute name="obj_pos">
        <dia:point val="4,23.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.985,23.135;15.615,23.165"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4,23.15"/>
        <dia:point val="15.6,23.15"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999999999999"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O17">
      <dia:attribute name="obj_pos">
        <dia:point val="4,18.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.985,18.135;15.615,18.165"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4,18.15"/>
        <dia:point val="15.6,18.15"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O18">
      <dia:attribute name="obj_pos">
        <dia:point val="5.15,19.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.135,19.135;15.615,19.165"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.15,19.15"/>
        <dia:point val="15.6,19.15"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O19">
      <dia:attribute name="obj_pos">
        <dia:point val="5.15,20.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.135,20.135;15.615,20.165"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.15,20.15"/>
        <dia:point val="15.6,20.15"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O20">
      <dia:attribute name="obj_pos">
        <dia:point val="5.15,21.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.135,21.135;15.615,21.165"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.15,21.15"/>
        <dia:point val="15.6,21.15"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O21">
      <dia:attribute name="obj_pos">
        <dia:point val="5.15,22.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.135,22.135;15.615,22.165"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.15,22.15"/>
        <dia:point val="15.6,22.15"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O22">
      <dia:attribute name="obj_pos">
        <dia:point val="5.15,18.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.145,18.145;5.155,23.155"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.15,18.15"/>
        <dia:point val="5.15,23.15"/>
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
        <dia:point val="8.8486,15.0875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.8336,15.0725;8.865,24.165"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.8486,15.0875"/>
        <dia:point val="8.85,24.15"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O24">
      <dia:attribute name="obj_pos">
        <dia:point val="14.4724,17.1644"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.4573,17.1494;14.4949,24.1619"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="14.4724,17.1644"/>
        <dia:point val="14.4799,24.1469"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O25">
      <dia:attribute name="obj_pos">
        <dia:point val="14.4986,23.1531"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.83127,23.1358;14.5159,24.1455"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="14.4986,23.1531"/>
        <dia:point val="8.8486,24.1281"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O26">
      <dia:attribute name="obj_pos">
        <dia:point val="11.0861,15.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.0709,15.0848;11.1138,16.1152"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.0861,15.1"/>
        <dia:point val="11.0986,16.1"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.30000000000000016"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O27">
      <dia:attribute name="obj_pos">
        <dia:point val="13.32,15.0839"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.3049,15.0687;13.3477,16.0991"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.32,15.0839"/>
        <dia:point val="13.3325,16.0839"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
      <dia:attribute name="line_style">
        <dia:enum val="4"/>
      </dia:attribute>
      <dia:attribute name="dashlength">
        <dia:real val="0.30000000000000016"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O28">
      <dia:attribute name="obj_pos">
        <dia:point val="14.4897,25.1687"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.4747,25.1537;18.4724,26.8587"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="14.4897,25.1687"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="3.9676353649698219"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="1.6749999999999972"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.029999999999999999"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O29">
      <dia:attribute name="obj_pos">
        <dia:point val="18.4522,25.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.4747,25.785;18.4672,25.815"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="18.4522,25.8"/>
        <dia:point val="14.4897,25.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O30">
      <dia:attribute name="obj_pos">
        <dia:point val="17.6611,25.7962"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.6459,25.7811;17.6876,26.8527"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.6611,25.7962"/>
        <dia:point val="17.6725,26.8375"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.029999999329447746"/>
      </dia:attribute>
    </dia:object>
  </dia:layer>
  <dia:layer name="文字" visible="true" active="true">
    <dia:object type="Standard - Text" version="1" id="O31">
      <dia:attribute name="obj_pos">
        <dia:point val="5.94541,6.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.94541,5.6825;13.9954,6.385"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#領 収 証 明 書 送 付 書#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.875"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.94541,6.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O32">
      <dia:attribute name="obj_pos">
        <dia:point val="8.41908,8.01466"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.41908,7.57466;9.41908,8.07716"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#長様#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.41908,8.01466"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O33">
      <dia:attribute name="obj_pos">
        <dia:point val="8.99364,9.61767"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.99364,9.35267;10.1936,9.65517"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#所 在 地#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.99364,9.61767"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O34">
      <dia:attribute name="obj_pos">
        <dia:point val="8.99364,10.7857"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.99364,10.5207;10.1936,10.8232"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#名    称#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.99364,10.7857"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O35">
      <dia:attribute name="obj_pos">
        <dia:point val="8.99364,11.6594"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.99364,11.3944;10.4936,11.6969"/>
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
            <dia:point val="8.99364,11.6594"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O36">
      <dia:attribute name="obj_pos">
        <dia:point val="8.99364,12.309"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.99364,12.044;10.1936,12.3465"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#電話番号#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.99364,12.309"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O37">
      <dia:attribute name="obj_pos">
        <dia:point val="7.03162,11.1357"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.03162,10.8707;8.53162,11.1732"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#医療機関等#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.03162,11.1357"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O38">
      <dia:attribute name="obj_pos">
        <dia:point val="17.4913,11.6922"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.4913,11.4272;17.7913,11.7297"/>
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
            <dia:point val="17.4913,11.6922"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O39">
      <dia:attribute name="obj_pos">
        <dia:point val="4.53834,23.8853"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.53834,23.4878;8.36334,23.9403"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#郵     送     料 #</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.53834,23.8853"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O40">
      <dia:attribute name="obj_pos">
        <dia:point val="5.50468,21.794"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.50468,21.4314;8.71474,21.8641"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#その他１（※１）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.50468,21.794"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O41">
      <dia:attribute name="obj_pos">
        <dia:point val="3.15,13.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.15,13.1225;16.84,13.495"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#福祉医療費助成制度の対象となる者の領収証明書を下記のとおり送付いたします。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.46250000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.15,13.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="4.4,19.6813"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.4,19.2838;4.85,22.5488"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#内




訳#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.4,19.6813"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="7.80362,3.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.80362,2.61;17.3036,3.1125"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#医　　療　　機　　関　　コ　　ー　　ド#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.80362,3.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="6.97633,3.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.97633,3.2725;8.45633,3.645"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#県 番 号#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.46250000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.97633,3.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="9.1,3.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.1,3.2725;9.84,3.645"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#区分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.46250000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.1,3.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="10.35,3.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.35,3.2725;12.2,3.645"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#地区コード#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.46250000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.35,3.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="13.867,3.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.867,3.2725;15.717,3.645"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#連      番#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.46250000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.867,3.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="17.45,3.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.45,3.2725;18.19,3.645"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ＣＤ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.46250000000000002"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.45,3.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="14.8237,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.8237,17.5025;15.2737,17.955"/>
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
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.8237,17.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="14.8237,18.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.8237,18.5025;15.2737,18.955"/>
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
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.8237,18.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="14.8237,19.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.8237,19.5025;15.2737,19.955"/>
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
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.8237,19.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="14.8237,20.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.8237,20.5025;15.2737,20.955"/>
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
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.8237,20.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="14.8237,21.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.8237,21.5025;15.2737,21.955"/>
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
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.8237,21.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="14.8237,22.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.8237,22.4525;15.2737,22.905"/>
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
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.8237,22.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="14.8237,23.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.8237,23.5025;15.2737,23.955"/>
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
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.8237,23.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="9.85,4.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.05,3.995;9.85,4.7975"/>
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
            <dia:real val="1"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.85,4.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O57">
      <dia:attribute name="obj_pos">
        <dia:point val="7.45,4.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.65,3.995;7.45,4.7975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="1"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.45,4.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O58">
      <dia:attribute name="obj_pos">
        <dia:point val="8.65,4.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.85,3.995;8.65,4.7975"/>
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
            <dia:real val="1"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.65,4.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O59">
      <dia:attribute name="obj_pos">
        <dia:point val="5.50758,22.7994"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.50758,22.4368;8.71764,22.8695"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#その他２（※２）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.50758,22.7994"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="8.08615,15.7656"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.08615,15.3681;8.53615,15.8206"/>
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
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.08615,15.7656"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
      <dia:object type="Standard - Text" version="1" id="O61">
        <dia:attribute name="obj_pos">
          <dia:point val="4.46502,16.8117"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.46502,16.4142;4.91502,16.8667"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#送#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.5625"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.46502,16.8117"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="8.12921,16.8117"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="8.12921,16.4142;8.57921,16.8667"/>
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
              <dia:real val="0.5625"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="8.12921,16.8117"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="6.29711,16.8117"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.29711,16.4142;6.74711,16.8667"/>
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
              <dia:real val="0.5625"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.29711,16.8117"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
      <dia:object type="Standard - Text" version="1" id="O64">
        <dia:attribute name="obj_pos">
          <dia:point val="4.42668,17.7798"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.42668,17.3823;4.87668,17.8348"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#証#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.5625"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.42668,17.7798"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="5.64908,17.7798"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="5.64908,17.3823;6.09908,17.8348"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#明#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.5625"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="5.64908,17.7798"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="6.87147,17.7798"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.87147,17.3823;7.32147,17.8348"/>
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
              <dia:real val="0.5625"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.87147,17.7798"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="8.09387,17.7798"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="8.09387,17.3823;8.54387,17.8348"/>
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
              <dia:real val="0.5625"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="8.09387,17.7798"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
    <dia:object type="Standard - Text" version="1" id="O68">
      <dia:attribute name="obj_pos">
        <dia:point val="3.12875,25.08"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.12875,24.805;9.69125,25.1175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※１その他１（伊勢市ー寡婦、紀宝町ー老人）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.38911388173646538"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.12875,25.08"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="3.14125,25.4925"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.14125,25.2175;13.7662,25.53"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※２その他２（津市ー妊産婦・精神障害者、御浜町ー寡婦、紀宝町ー寡婦）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.38911388173646538"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.14125,25.4925"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
      <dia:object type="Standard - Text" version="1" id="O70">
        <dia:attribute name="obj_pos">
          <dia:point val="5.47008,18.8293"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="5.47008,18.4768;5.87008,18.8793"/>
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
              <dia:real val="0.5"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="5.47008,18.8293"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="6.35904,18.8293"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.35904,18.4768;6.75904,18.8793"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#が#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.5"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.35904,18.8293"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="7.24799,18.8293"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="7.24799,18.4768;7.64799,18.8793"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#い#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.5"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.24799,18.8293"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="8.13695,18.8293"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="8.13695,18.4768;8.53695,18.8793"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#者#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.5"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="8.13695,18.8293"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
      <dia:object type="Standard - Text" version="1" id="O74">
        <dia:attribute name="obj_pos">
          <dia:point val="5.47008,19.7987"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="5.47008,19.4462;5.87008,19.8487"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#一#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.5"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="5.47008,19.7987"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="6.00845,19.7987"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.00845,19.4462;6.40845,19.8487"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#人#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.5"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.00845,19.7987"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="6.54683,19.8087"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.54683,19.4562;6.94683,19.8587"/>
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
              <dia:real val="0.5"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.54683,19.8087"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="7.0852,19.7987"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="7.0852,19.4462;7.4852,19.8487"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#家#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.5"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.0852,19.7987"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="7.62358,19.8087"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="7.62358,19.4562;8.02358,19.8587"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#庭#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.5"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.62358,19.8087"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="8.16195,19.7987"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="8.16195,19.4462;8.56195,19.8487"/>
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
              <dia:real val="0.5"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="8.16195,19.7987"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
      <dia:object type="Standard - Text" version="1" id="O80">
        <dia:attribute name="obj_pos">
          <dia:point val="5.47008,20.8448"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="5.47008,20.4923;5.87008,20.8948"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#子#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.5"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="5.47008,20.8448"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="6.82539,20.8549"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.82539,20.5024;7.22539,20.9049"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ど#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.5"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.82539,20.8549"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="8.1807,20.8448"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="8.1807,20.4923;8.5807,20.8948"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#も#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.5"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="8.1807,20.8448"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
    <dia:object type="Standard - Text" version="1" id="O83">
      <dia:attribute name="obj_pos">
        <dia:point val="15.0585,25.5912"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.0585,25.2912;17.7785,25.6337"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※証明書料件数欄#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.42474444557411878"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.0585,25.5912"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="17.9085,26.4687"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.9085,26.1687;18.2485,26.5112"/>
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
            <dia:real val="0.42474444557411878"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.9085,26.4687"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
          <dia:point val="4.41896,15.7656"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.41896,15.3681;4.86896,15.8206"/>
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
              <dia:real val="0.5625"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.41896,15.7656"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="5.39688,15.7656"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="5.39688,15.3681;5.84688,15.8206"/>
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
              <dia:real val="0.5625"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="5.39688,15.7656"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="6.37479,15.7656"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="6.37479,15.3681;6.82479,15.8206"/>
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
              <dia:real val="0.5625"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="6.37479,15.7656"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="7.35271,15.7656"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="7.35271,15.3681;7.80271,15.8206"/>
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
              <dia:real val="0.5625"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.35271,15.7656"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
  <dia:layer name="差し込み" visible="true">
    <dia:object type="Embed - Text" version="1" id="O89">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1076,8.01466"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.8576,7.61716;8.3576,8.06966"/>
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
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1076,8.01466"/>
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
      <dia:attribute name="embed_id">
        <dia:string>#CITYNAME[0]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O90">
      <dia:attribute name="obj_pos">
        <dia:point val="11.05,4.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.25,3.995;11.05,4.7975"/>
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
            <dia:real val="1"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.05,4.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HOSPCD-G[0].HOSPCD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O91">
      <dia:attribute name="obj_pos">
        <dia:point val="14.1,18.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.1,18.46;14.1,18.9625"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.1,18.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KENSU-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O92">
      <dia:attribute name="obj_pos">
        <dia:point val="14.1,19.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.1,19.46;14.1,19.9625"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.1,19.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KENSU-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O93">
      <dia:attribute name="obj_pos">
        <dia:point val="14.1,20.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.1,20.46;14.1,20.9625"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.1,20.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KENSU-TBL[2].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O94">
      <dia:attribute name="obj_pos">
        <dia:point val="14.1,21.847"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.1,21.407;14.1,21.9095"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.1,21.847"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KENSU-TBL[3].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O95">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3113,16.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.3113,16.41;14.3113,16.9125"/>
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
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3113,16.85"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#SEIYM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O96">
      <dia:attribute name="obj_pos">
        <dia:point val="12.3,4.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.5,3.995;12.3,4.7975"/>
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
            <dia:real val="1"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.3,4.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HOSPCD-G[1].HOSPCD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O97">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,4.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.65,3.995;13.45,4.7975"/>
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
            <dia:real val="1"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.45,4.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HOSPCD-G[2].HOSPCD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O98">
      <dia:attribute name="obj_pos">
        <dia:point val="14.65,4.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.85,3.995;14.65,4.7975"/>
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
            <dia:real val="1"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.65,4.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HOSPCD-G[3].HOSPCD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O99">
      <dia:attribute name="obj_pos">
        <dia:point val="15.85,4.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.05,3.995;15.85,4.7975"/>
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
            <dia:real val="1"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.85,4.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HOSPCD-G[4].HOSPCD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O100">
      <dia:attribute name="obj_pos">
        <dia:point val="17.05,4.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.25,3.995;17.05,4.7975"/>
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
            <dia:real val="1"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.05,4.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HOSPCD-G[5].HOSPCD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O101">
      <dia:attribute name="obj_pos">
        <dia:point val="18.25,4.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.45,3.995;18.25,4.7975"/>
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
            <dia:real val="1"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.25,4.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HOSPCD-G[6].HOSPCD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O102">
      <dia:attribute name="obj_pos">
        <dia:point val="14.039,7.31497"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.039,6.96247;17.839,7.36497"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#平成   年   月   日#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.039,7.31497"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#SEIYMD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O103">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7036,9.60247"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7036,9.33747;12.2036,9.63997"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#〒999-9999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7036,9.60247"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#POST#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O104">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7036,9.9735"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7036,9.7085;18.2036,10.386"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああああああああああああああああああああああ
あああああああああああああああああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7036,9.9735"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O105">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7036,10.8039"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7036,10.5389;18.2036,11.2164"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああああああああああああああああああああああ
あああああああああああああああああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7036,10.8039"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O106">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7036,11.6873"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7036,11.4223;16.7036,11.7248"/>
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
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7036,11.6873"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#KAISETU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O107">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7036,12.3057"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7036,11.9532;13.7036,12.3557"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#123456789012345#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.7036,12.3057"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#TEL#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O108">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0987,22.8084"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.0987,22.3684;14.0987,22.8709"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0987,22.8084"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KENSU-TBL[4].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O109">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0843,17.8614"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.0843,17.4214;14.0843,17.9239"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#9999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0843,17.8614"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KENSU-TBL[5].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O110">
      <dia:attribute name="obj_pos">
        <dia:point val="9.8986,15.9125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.8986,15.24;10.2811,16.005"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#0#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.95532222509836862"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.8986,15.9125"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#CITYCD[0]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O111">
      <dia:attribute name="obj_pos">
        <dia:point val="12.0173,15.9062"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.0173,15.2337;12.3998,15.9987"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#0#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.95532222509836862"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.0173,15.9062"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#CITYCD[1]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O112">
      <dia:attribute name="obj_pos">
        <dia:point val="14.2486,15.925"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.2486,15.2525;14.6311,16.0175"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.95532222509836862"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.2486,15.925"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#CITYCD[2]#</dia:string>
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
