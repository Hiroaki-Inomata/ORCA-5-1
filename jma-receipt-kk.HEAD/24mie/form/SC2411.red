<?xml version="1.0" encoding="UTF-8"?>
<dia:diagram xmlns:dia="http://www.lysator.liu.se/~alla/dia/">
  <dia:dictionarydata>
    <element name="SEIYM" occurs="1">
      <appinfo>
        <embed object="text" length="16"/>
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
    <element name="TEL" occurs="1">
      <appinfo>
        <embed object="text" length="24"/>
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
    <element name="KENSU-TBL" occurs="6">
      <element name="KENSU" occurs="1">
        <appinfo>
          <embed object="text" length="7"/>
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
        <dia:point val="2.24382,9.32862"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.23382,9.31862;11.8916,12.9252"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="2.24382,9.32862"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="9.63781"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="3.58657"/>
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
        <dia:point val="2.24382,11.1219"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.23382,11.1119;11.8916,11.1319"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.24382,11.1219"/>
        <dia:point val="11.8816,11.1219"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O0" connection="3"/>
        <dia:connection handle="1" to="O0" connection="4"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O2">
      <dia:attribute name="obj_pos">
        <dia:point val="2.24468,10.2482"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.23467,10.2382;11.8909,10.2662"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.24468,10.2482"/>
        <dia:point val="11.8809,10.2562"/>
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
        <dia:point val="2.24468,12.015"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.23467,12.005;11.8909,12.033"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.24468,12.015"/>
        <dia:point val="11.8809,12.023"/>
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
        <dia:point val="5.5212,9.32862"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.51115,9.31857;5.54892,12.9252"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.5212,9.32862"/>
        <dia:point val="5.53887,12.9152"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O5">
      <dia:attribute name="obj_pos">
        <dia:point val="2.27031,15.0717"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.26031,15.0617;17.775,22.3863"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="2.27031,15.0717"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="15.4947"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="7.3045900000000001"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O6">
      <dia:attribute name="obj_pos">
        <dia:point val="2.26855,16.1176"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.25854,16.0854;17.7666,16.1276"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.26855,16.1176"/>
        <dia:point val="17.7566,16.0954"/>
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
        <dia:point val="2.26855,17.1595"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.25855,17.1495;17.7762,17.1743"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.26855,17.1595"/>
        <dia:point val="17.7662,17.1643"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O8">
      <dia:attribute name="obj_pos">
        <dia:point val="2.26855,18.2014"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.25855,18.1914;17.7762,18.2167"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.26855,18.2014"/>
        <dia:point val="17.7662,18.2067"/>
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
        <dia:point val="2.26855,19.2434"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.25855,19.2334;17.7762,19.2591"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.26855,19.2434"/>
        <dia:point val="17.7662,19.2491"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O10">
      <dia:attribute name="obj_pos">
        <dia:point val="2.26855,20.2853"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.25855,20.2753;17.7762,20.3015"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.26855,20.2853"/>
        <dia:point val="17.7662,20.2915"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O11">
      <dia:attribute name="obj_pos">
        <dia:point val="2.26855,21.3272"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.25855,21.3172;17.7762,21.3439"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.26855,21.3272"/>
        <dia:point val="17.7662,21.3339"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O12">
      <dia:attribute name="obj_pos">
        <dia:point val="7.43509,15.0733"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.42132,15.0633;7.4451,22.3867"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="7.43509,15.0733"/>
        <dia:point val="7.43133,22.3767"/>
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
        <dia:point val="12.5999,15.0757"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.5861,15.0657;12.6099,22.3891"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.5999,15.0757"/>
        <dia:point val="12.5961,22.3791"/>
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
    <dia:object type="Standard - Text" version="1" id="O14">
      <dia:attribute name="obj_pos">
        <dia:point val="10.5131,5.87209"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5131,5.60709;11.5631,5.90959"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#住   所#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.5131,5.87209"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O15">
      <dia:attribute name="obj_pos">
        <dia:point val="10.5131,7.19913"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5131,6.93413;11.5631,7.23663"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#名   称#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.5131,7.19913"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O16">
      <dia:attribute name="obj_pos">
        <dia:point val="10.5131,8.01571"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.5131,7.75071;11.7131,8.05321"/>
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
            <dia:point val="10.5131,8.01571"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O17">
      <dia:attribute name="obj_pos">
        <dia:point val="2.30194,13.609"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.30194,13.344;12.5019,13.6465"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※該当する点数表区分又は療養費区分及び提出方法を○で囲んでください。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.30194,13.609"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O18">
      <dia:attribute name="obj_pos">
        <dia:point val="2.46996,6.6189"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.46996,6.3089;8.41996,6.6614"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#三重県国民健康保険団体連合会    様#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.46996,6.6189"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O19">
      <dia:attribute name="obj_pos">
        <dia:point val="10.973,3.53668"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.973,3.09668;16.973,3.59918"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#福祉医療費電子媒体送付書#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.973,3.53668"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O20">
      <dia:attribute name="obj_pos">
        <dia:point val="9.11357,3.53668"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.11357,3.09668;10.6136,3.59918"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#送付分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.11357,3.53668"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O21">
      <dia:attribute name="obj_pos">
        <dia:point val="2.76784,9.91557"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.76784,9.65057;5.16784,9.95307"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#医療機関等コード#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.76784,9.91557"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
      <dia:object type="Standard - Text" version="1" id="O22">
        <dia:attribute name="obj_pos">
          <dia:point val="2.76784,10.8166"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.76784,10.5516;3.06784,10.8541"/>
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
              <dia:point val="2.76784,10.8166"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O23">
        <dia:attribute name="obj_pos">
          <dia:point val="3.26254,10.8166"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3.26254,10.5516;3.56254,10.8541"/>
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
              <dia:point val="3.26254,10.8166"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O24">
        <dia:attribute name="obj_pos">
          <dia:point val="3.75724,10.8166"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3.75724,10.5516;4.05724,10.8541"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#表#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="3.75724,10.8166"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O25">
        <dia:attribute name="obj_pos">
          <dia:point val="4.25194,10.8166"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.25194,10.5516;4.55194,10.8541"/>
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
              <dia:point val="4.25194,10.8166"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O26">
        <dia:attribute name="obj_pos">
          <dia:point val="4.74664,10.8166"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.74664,10.5516;5.04664,10.8541"/>
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
              <dia:point val="4.74664,10.8166"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
      <dia:object type="Standard - Text" version="1" id="O27">
        <dia:attribute name="obj_pos">
          <dia:point val="2.78551,11.6824"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.78551,11.4174;3.08551,11.7199"/>
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
              <dia:point val="2.78551,11.6824"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O28">
        <dia:attribute name="obj_pos">
          <dia:point val="3.28021,11.6824"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3.28021,11.4174;3.58021,11.7199"/>
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
              <dia:point val="3.28021,11.6824"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O29">
        <dia:attribute name="obj_pos">
          <dia:point val="3.77491,11.6824"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3.77491,11.4174;4.07491,11.7199"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#費#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="3.77491,11.6824"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O30">
        <dia:attribute name="obj_pos">
          <dia:point val="4.26961,11.6824"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.26961,11.4174;4.56961,11.7199"/>
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
              <dia:point val="4.26961,11.6824"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
        <dia:attribute name="valign">
          <dia:enum val="3"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="1" id="O31">
        <dia:attribute name="obj_pos">
          <dia:point val="4.76431,11.6824"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.76431,11.4174;5.06431,11.7199"/>
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
              <dia:point val="4.76431,11.6824"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
      <dia:object type="Standard - Text" version="1" id="O32">
        <dia:attribute name="obj_pos">
          <dia:point val="2.78551,12.5481"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.78551,12.2831;3.08551,12.5856"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#媒#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.78551,12.5481"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="3.44511,12.5481"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3.44511,12.2831;3.74511,12.5856"/>
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
              <dia:point val="3.44511,12.5481"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="4.10471,12.5481"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.10471,12.2831;4.40471,12.5856"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#枚#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takao明朝" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.10471,12.5481"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
          <dia:point val="4.76431,12.5481"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.76431,12.2831;5.06431,12.5856"/>
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
              <dia:point val="4.76431,12.5481"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
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
    <dia:object type="Standard - Text" version="1" id="O36">
      <dia:attribute name="obj_pos">
        <dia:point val="5.91272,10.8166"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.91272,10.5516;10.7127,10.8541"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#医科 ・ 歯科 ・ 調剤 ・ 訪問看護#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.91272,10.8166"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="5.91272,11.6824"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.91272,11.4174;9.81272,11.7199"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#柔整 ・ 鍼灸 ・ マッサージ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.91272,11.6824"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="9.53463,12.5481"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.53463,12.2831;9.83463,12.5856"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#枚#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.53463,12.5481"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="4.24382,15.7371"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.24382,15.4271;5.64382,15.7796"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#助成種別#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.24382,15.7371"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.95406,14.7301"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.95406,14.4201;4.35406,14.7726"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#《内訳》#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.95406,14.7301"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="9.4205,15.7239"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.4205,15.4139;10.8205,15.7664"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#証明件数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.4205,15.7239"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="14.58,15.5119"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.58,15.2019;15.98,15.9919"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#決定件数
#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.58,15.5119"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="14.0676,15.9359"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0676,15.6259;16.5176,16.4159"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※連合会使用欄
#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.0676,15.9359"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="3.23675,16.7663"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.23675,16.4563;5.33675,16.8088"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１．障がい者#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.23675,16.7663"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="3.23675,17.811"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.23675,17.501;6.03675,17.8535"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２．一人親家庭等#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.23675,17.811"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="3.23675,18.8557"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.23675,18.5457;4.98675,18.8982"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#３．子ども#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.23675,18.8557"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="3.23675,19.9004"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.23675,19.5904;6.73675,19.9429"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#４．その他１（※１）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.23675,19.9004"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="3.23675,20.9451"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.23675,20.6351;6.73675,20.9876"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#５．その他２（※２）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.23675,20.9451"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="3.90813,21.9345"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.90813,21.6245;5.65813,21.977"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#合      計#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.4375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.90813,21.9345"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.33569,22.8311"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.33569,22.5661;7.73569,22.8686"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※助成種別証明件数をご記入ください。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.33569,22.8311"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.33569,23.3965"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.33569,23.1315;9.98569,23.434"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※１助成種別 その他１（伊勢市ー寡婦、紀宝町ー老人）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.33569,23.3965"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="2.33569,23.9795"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.33569,23.7145;13.8857,24.017"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※２助成種別 その他２（津市ー助産婦・精神障害者、御浜町ー寡婦、紀宝町ー寡婦）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.33569,23.9795"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="11.8912,16.7579"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.8912,16.4929;12.1912,16.7954"/>
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
            <dia:point val="11.8912,16.7579"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="11.8912,17.8029"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.8912,17.5379;12.1912,17.8404"/>
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
            <dia:point val="11.8912,17.8029"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="11.8912,18.8479"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.8912,18.5829;12.1912,18.8854"/>
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
            <dia:point val="11.8912,18.8479"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="11.8912,19.893"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.8912,19.628;12.1912,19.9305"/>
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
            <dia:point val="11.8912,19.893"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="11.8912,20.938"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.8912,20.673;12.1912,20.9755"/>
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
            <dia:point val="11.8912,20.938"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="11.8912,21.9831"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.8912,21.7181;12.1912,22.0206"/>
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
            <dia:point val="11.8912,21.9831"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="17.1562,16.718"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1562,16.453;17.4562,16.7555"/>
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
            <dia:point val="17.1562,16.718"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="17.1562,17.7631"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1562,17.4981;17.4562,17.8006"/>
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
            <dia:point val="17.1562,17.7631"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="17.1562,18.8081"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1562,18.5431;17.4562,18.8456"/>
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
            <dia:point val="17.1562,18.8081"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="17.1562,19.8532"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1562,19.5882;17.4562,19.8907"/>
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
            <dia:point val="17.1562,19.8532"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="17.1562,20.8982"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1562,20.6332;17.4562,20.9357"/>
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
            <dia:point val="17.1562,20.8982"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
        <dia:point val="17.1562,21.9432"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1562,21.6782;17.4562,21.9807"/>
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
            <dia:point val="17.1562,21.9432"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
    <dia:object type="Embed - Text" version="1" id="O65">
      <dia:attribute name="obj_pos">
        <dia:point val="6.88039,9.98269"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.88039,9.54269;10.3804,10.0452"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#９９９９９９９#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takao明朝" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.88039,9.98269"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O66">
      <dia:attribute name="obj_pos">
        <dia:point val="11.3458,16.8098"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.5958,16.3698;11.3458,16.8723"/>
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
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.3458,16.8098"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
    <dia:object type="Embed - Text" version="1" id="O67">
      <dia:attribute name="obj_pos">
        <dia:point val="11.3458,17.8557"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.5958,17.4157;11.3458,17.9182"/>
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
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.3458,17.8557"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
    <dia:object type="Embed - Text" version="1" id="O68">
      <dia:attribute name="obj_pos">
        <dia:point val="11.3458,18.9016"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.5958,18.4616;11.3458,18.9641"/>
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
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.3458,18.9016"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
    <dia:object type="Embed - Text" version="1" id="O69">
      <dia:attribute name="obj_pos">
        <dia:point val="11.3458,19.9476"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.5958,19.5076;11.3458,20.0101"/>
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
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.3458,19.9476"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
    <dia:object type="Embed - Text" version="1" id="O70">
      <dia:attribute name="obj_pos">
        <dia:point val="5.13895,3.55415"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.13895,3.11415;9.13895,3.61665"/>
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
            <dia:point val="5.13895,3.55415"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O71">
      <dia:attribute name="obj_pos">
        <dia:point val="11.799,5.83922"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.799,5.57422;13.299,5.87672"/>
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
            <dia:point val="11.799,5.83922"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O72">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7813,6.22792"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.7813,5.96292;19.2813,6.64042"/>
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
            <dia:point val="11.7813,6.22792"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O73">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7637,7.19966"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.7637,6.93466;19.2637,7.61216"/>
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
            <dia:point val="11.7637,7.19966"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O74">
      <dia:attribute name="obj_pos">
        <dia:point val="11.799,8.03008"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.799,7.67758;14.799,8.08008"/>
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
            <dia:point val="11.799,8.03008"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O75">
      <dia:attribute name="obj_pos">
        <dia:point val="11.3458,20.9935"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.5958,20.5535;11.3458,21.056"/>
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
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.3458,20.9935"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
    <dia:object type="Embed - Text" version="1" id="O76">
      <dia:attribute name="obj_pos">
        <dia:point val="11.3458,22.0388"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.5958,21.5988;11.3458,22.1013"/>
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
            <dia:real val="0.625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.3458,22.0388"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
    <dia:object type="Embed - Text" version="1" id="O77">
      <dia:attribute name="obj_pos">
        <dia:point val="5.96219,10.8964"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.96219,10.4564;6.46219,10.9589"/>
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
            <dia:point val="5.96219,10.8964"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O78">
      <dia:attribute name="obj_pos">
        <dia:point val="7.23781,10.8964"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.23781,10.4564;7.73781,10.9589"/>
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
            <dia:point val="7.23781,10.8964"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O79">
      <dia:attribute name="obj_pos">
        <dia:point val="8.36289,10.8964"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.36289,10.4564;8.86289,10.9589"/>
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
            <dia:point val="8.36289,10.8964"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
    <dia:object type="Embed - Text" version="1" id="O80">
      <dia:attribute name="obj_pos">
        <dia:point val="9.8397,10.8964"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.8397,10.4564;10.3397,10.9589"/>
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
            <dia:point val="9.8397,10.8964"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
  </dia:layer>
</dia:diagram>
