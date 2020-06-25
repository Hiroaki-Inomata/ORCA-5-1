<?xml version="1.0" encoding="UTF-8"?>
<dia:diagram xmlns:dia="http://www.lysator.liu.se/~alla/dia/">
  <dia:dictionarydata>
    <dia:element name="HOSPCD" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="20"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="ROU-TBL" occurs="1">
      <dia:element name="ROU-TBL1" occurs="2">
        <dia:element name="ROU-G" occurs="3">
          <dia:element name="ROU-KENSU" occurs="1">
            <dia:appinfo>
              <dia:embed object="string" length="6"/>
            </dia:appinfo>
          </dia:element>
          <dia:element name="ROU-NISSU" occurs="1">
            <dia:appinfo>
              <dia:embed object="string" length="6"/>
            </dia:appinfo>
          </dia:element>
          <dia:element name="ROU-TENSU" occurs="1">
            <dia:appinfo>
              <dia:embed object="string" length="10"/>
            </dia:appinfo>
          </dia:element>
          <dia:element name="ROU-ITBFTN" occurs="1">
            <dia:appinfo>
              <dia:embed object="string" length="9"/>
            </dia:appinfo>
          </dia:element>
        </dia:element>
      </dia:element>
    </dia:element>
    <dia:element name="KOH-TBL" occurs="1">
      <dia:element name="KOH-TBL1" occurs="3">
        <dia:element name="KOH-G" occurs="6">
          <dia:element name="KOHKBN" occurs="1">
            <dia:appinfo>
              <dia:embed object="string" length="44"/>
            </dia:appinfo>
          </dia:element>
          <dia:element name="KOH-KENSU" occurs="1">
            <dia:appinfo>
              <dia:embed object="string" length="6"/>
            </dia:appinfo>
          </dia:element>
          <dia:element name="KOH-TENSU" occurs="1">
            <dia:appinfo>
              <dia:embed object="string" length="10"/>
            </dia:appinfo>
          </dia:element>
          <dia:element name="KOH-ITBFTN" occurs="1">
            <dia:appinfo>
              <dia:embed object="string" length="9"/>
            </dia:appinfo>
          </dia:element>
        </dia:element>
      </dia:element>
    </dia:element>
    <dia:element name="KENSUG" occurs="2">
      <dia:appinfo>
        <dia:embed object="string" length="6"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="GKENSU" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="7"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="TITLE" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="50"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="BKO-TBL" occurs="10">
      <dia:element name="BKO-KENSU" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="6"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="BKO-NISSU" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="6"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="BKO-ITBFTN" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="9"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="BKO-KOJYO" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="9"/>
        </dia:appinfo>
      </dia:element>
    </dia:element>
    <dia:element name="TITLE1" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="10"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="AVG-TBL" occurs="3">
      <dia:element name="AVG" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="11"/>
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
  <dia:layer name="背景" visible="true">
    <dia:object type="Standard - Box" version="0" id="O0">
      <dia:attribute name="obj_pos">
        <dia:point val="1.7,26.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.69,26.54;7.31,27.76"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="1.7,26.55"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="5.6"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="1.2"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.02"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Box" version="0" id="O1">
      <dia:attribute name="obj_pos">
        <dia:point val="1.2,1.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.195,1.695;19.805,28.005"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="1.2,1.7"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="18.6"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="26.3"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O2">
      <dia:attribute name="obj_pos">
        <dia:point val="13.5,2.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.495,2.295;19.305,2.305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.5,2.3"/>
        <dia:point val="19.3,2.3"/>
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
        <dia:point val="1.7,26.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.695,26.395;19.305,26.405"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.7,26.4"/>
        <dia:point val="19.3,26.4"/>
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
        <dia:point val="1.7,18.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.695,18.495;1.705,26.405"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.7,18.5"/>
        <dia:point val="1.7,26.4"/>
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
        <dia:point val="19.3,18.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.295,18.495;19.305,26.405"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.3,18.5"/>
        <dia:point val="19.3,26.4"/>
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
        <dia:point val="5.9,3.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,3.395;19.305,3.405"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,3.4"/>
        <dia:point val="19.3,3.4"/>
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
        <dia:point val="5.9,4.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,4.695;19.305,4.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,4.7"/>
        <dia:point val="19.3,4.7"/>
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
        <dia:point val="5.9,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,5.195;19.305,5.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,5.2"/>
        <dia:point val="19.3,5.2"/>
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
        <dia:point val="5.9,5.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,5.695;19.305,5.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,5.7"/>
        <dia:point val="19.3,5.7"/>
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
        <dia:point val="5.9,6.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,6.195;19.305,6.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,6.2"/>
        <dia:point val="19.3,6.2"/>
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
        <dia:point val="5.9,6.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,6.695;19.305,6.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,6.7"/>
        <dia:point val="19.3,6.7"/>
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
        <dia:point val="5.9,7.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,7.195;19.305,7.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,7.2"/>
        <dia:point val="19.3,7.2"/>
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
        <dia:point val="5.9,7.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,7.695;19.305,7.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,7.7"/>
        <dia:point val="19.3,7.7"/>
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
        <dia:point val="5.9,8.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,8.195;19.305,8.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,8.2"/>
        <dia:point val="19.3,8.2"/>
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
        <dia:point val="5.9,8.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,8.695;19.305,8.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,8.7"/>
        <dia:point val="19.3,8.7"/>
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
        <dia:point val="5.9,9.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,9.195;19.305,9.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,9.2"/>
        <dia:point val="19.3,9.2"/>
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
        <dia:point val="5.9,9.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,9.695;19.305,9.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,9.7"/>
        <dia:point val="19.3,9.7"/>
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
        <dia:point val="5.9,11.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,11.195;19.305,11.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,11.2"/>
        <dia:point val="19.3,11.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
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
    <dia:object type="Standard - Line" version="0" id="O19">
      <dia:attribute name="obj_pos">
        <dia:point val="5.9,12.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,12.695;19.305,12.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,12.7"/>
        <dia:point val="19.3,12.7"/>
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
        <dia:point val="5.9,14.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,14.195;19.305,14.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,14.2"/>
        <dia:point val="19.3,14.2"/>
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
        <dia:point val="5.9,13.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,13.195;19.305,13.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,13.2"/>
        <dia:point val="19.3,13.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
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
    <dia:object type="Standard - Line" version="0" id="O22">
      <dia:attribute name="obj_pos">
        <dia:point val="5.9,12.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,12.195;19.305,12.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,12.2"/>
        <dia:point val="19.3,12.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
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
    <dia:object type="Standard - Line" version="0" id="O23">
      <dia:attribute name="obj_pos">
        <dia:point val="5.9,14.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,14.695;19.305,14.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,14.7"/>
        <dia:point val="19.3,14.7"/>
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
        <dia:point val="5.9,11.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,11.695;19.305,11.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,11.7"/>
        <dia:point val="19.3,11.7"/>
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
        <dia:point val="5.9,13.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,13.695;19.305,13.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,13.7"/>
        <dia:point val="19.3,13.7"/>
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
        <dia:point val="5.9,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,15.195;19.305,15.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,15.2"/>
        <dia:point val="19.3,15.2"/>
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
        <dia:point val="5.9,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,15.695;19.305,15.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,15.7"/>
        <dia:point val="19.3,15.7"/>
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
        <dia:point val="5.9,16.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,16.195;19.305,16.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,16.2"/>
        <dia:point val="19.3,16.2"/>
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
        <dia:point val="5.9,16.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,16.695;19.305,16.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,16.7"/>
        <dia:point val="19.3,16.7"/>
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
        <dia:point val="5.9,17.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,17.195;19.305,17.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,17.2"/>
        <dia:point val="19.3,17.2"/>
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
        <dia:point val="5.9,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,17.695;19.305,17.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,17.7"/>
        <dia:point val="19.3,17.7"/>
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
        <dia:point val="1.7,3.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.695,3.395;1.705,17.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.7,3.4"/>
        <dia:point val="1.7,17.7"/>
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
        <dia:point val="5.9,10.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,10.195;19.305,10.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,10.2"/>
        <dia:point val="19.3,10.2"/>
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
        <dia:point val="5.9,4.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,4.195;19.305,4.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,4.2"/>
        <dia:point val="19.3,4.2"/>
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
        <dia:point val="5.9,10.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,10.695;19.305,10.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,10.7"/>
        <dia:point val="19.3,10.7"/>
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
        <dia:point val="1.7,4.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.695,4.195;5.905,4.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.7,4.2"/>
        <dia:point val="5.9,4.2"/>
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
        <dia:point val="3.2,4.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,4.695;5.905,4.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,4.7"/>
        <dia:point val="5.9,4.7"/>
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
        <dia:point val="3.2,9.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,9.695;5.905,9.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,9.7"/>
        <dia:point val="5.9,9.7"/>
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
        <dia:point val="2.2,10.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,10.695;5.905,10.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,10.7"/>
        <dia:point val="5.9,10.7"/>
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
        <dia:point val="1.7,17.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.695,17.695;5.905,17.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.7,17.7"/>
        <dia:point val="5.9,17.7"/>
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
        <dia:point val="3.2,5.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,5.195;5.905,5.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,5.2"/>
        <dia:point val="5.9,5.2"/>
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
        <dia:point val="3.2,6.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,6.195;5.905,6.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,6.2"/>
        <dia:point val="5.9,6.2"/>
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
        <dia:point val="3.2,6.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,6.695;5.905,6.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,6.7"/>
        <dia:point val="5.9,6.7"/>
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
        <dia:point val="2.2,7.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,7.195;5.905,7.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,7.2"/>
        <dia:point val="5.9,7.2"/>
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
        <dia:point val="3.2,8.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,8.195;5.905,8.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,8.2"/>
        <dia:point val="5.9,8.2"/>
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
        <dia:point val="3.2,8.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,8.695;5.905,8.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,8.7"/>
        <dia:point val="5.9,8.7"/>
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
        <dia:point val="3.2,10.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,10.195;5.905,10.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,10.2"/>
        <dia:point val="5.9,10.2"/>
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
        <dia:point val="2.2,5.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,5.695;5.905,5.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,5.7"/>
        <dia:point val="5.9,5.7"/>
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
        <dia:point val="1.7,7.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.695,7.695;5.905,7.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.7,7.7"/>
        <dia:point val="5.9,7.7"/>
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
        <dia:point val="8.9,8.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,7.695;11.905,8.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,8.2"/>
        <dia:point val="11.9,7.7"/>
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
        <dia:point val="8.9,8.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,8.195;11.905,8.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,8.7"/>
        <dia:point val="11.9,8.2"/>
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
        <dia:point val="8.9,9.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,8.695;11.905,9.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,9.2"/>
        <dia:point val="11.9,8.7"/>
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
        <dia:point val="8.9,9.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,9.195;11.905,9.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,9.7"/>
        <dia:point val="11.9,9.2"/>
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
        <dia:point val="8.9,10.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,9.695;11.905,10.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,10.2"/>
        <dia:point val="11.9,9.7"/>
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
        <dia:point val="8.9,10.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,10.195;11.905,10.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,10.7"/>
        <dia:point val="11.9,10.2"/>
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
        <dia:point val="8.9,11.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,10.695;11.905,11.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,11.2"/>
        <dia:point val="11.9,10.7"/>
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
        <dia:point val="8.9,11.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,11.195;11.905,11.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,11.7"/>
        <dia:point val="11.9,11.2"/>
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
        <dia:point val="8.9,12.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,11.695;11.905,12.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,12.2"/>
        <dia:point val="11.9,11.7"/>
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
        <dia:point val="8.9,12.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,12.195;11.905,12.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,12.7"/>
        <dia:point val="11.9,12.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O60">
      <dia:attribute name="obj_pos">
        <dia:point val="8.9,13.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,12.695;11.905,13.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,13.2"/>
        <dia:point val="11.9,12.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O61">
      <dia:attribute name="obj_pos">
        <dia:point val="8.9,13.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,13.195;11.905,13.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,13.7"/>
        <dia:point val="11.9,13.2"/>
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
        <dia:point val="8.9,14.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,13.695;11.905,14.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,14.2"/>
        <dia:point val="11.9,13.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O63">
      <dia:attribute name="obj_pos">
        <dia:point val="8.9,14.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,14.195;11.905,14.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,14.7"/>
        <dia:point val="11.9,14.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O64">
      <dia:attribute name="obj_pos">
        <dia:point val="8.9,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,14.695;11.905,15.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,15.2"/>
        <dia:point val="11.9,14.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O65">
      <dia:attribute name="obj_pos">
        <dia:point val="8.9,16.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,16.195;11.905,16.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,16.7"/>
        <dia:point val="11.9,16.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O66">
      <dia:attribute name="obj_pos">
        <dia:point val="11.9,17.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.895,16.695;15.705,17.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.9,17.2"/>
        <dia:point val="15.7,16.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O67">
      <dia:attribute name="obj_pos">
        <dia:point val="3.2,4.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,4.195;3.205,7.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,4.2"/>
        <dia:point val="3.2,7.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O68">
      <dia:attribute name="obj_pos">
        <dia:point val="3.2,7.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,7.695;3.205,16.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,7.7"/>
        <dia:point val="3.2,16.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O69">
      <dia:attribute name="obj_pos">
        <dia:point val="3.2,13.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,13.195;5.905,13.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,13.2"/>
        <dia:point val="5.9,13.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
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
    <dia:object type="Standard - Line" version="0" id="O70">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,13.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,13.695;5.905,13.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,13.7"/>
        <dia:point val="5.9,13.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O71">
      <dia:attribute name="obj_pos">
        <dia:point val="3.2,11.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,11.195;5.905,11.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,11.2"/>
        <dia:point val="5.9,11.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
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
    <dia:object type="Standard - Line" version="0" id="O72">
      <dia:attribute name="obj_pos">
        <dia:point val="3.2,11.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,11.695;5.905,11.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,11.7"/>
        <dia:point val="5.9,11.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O73">
      <dia:attribute name="obj_pos">
        <dia:point val="3.2,12.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,12.195;5.905,12.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,12.2"/>
        <dia:point val="5.9,12.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
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
      <dia:connections>
        <dia:connection handle="0" to="O68" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O74">
      <dia:attribute name="obj_pos">
        <dia:point val="3.2,12.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,12.695;5.905,12.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,12.7"/>
        <dia:point val="5.9,12.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O75">
      <dia:attribute name="obj_pos">
        <dia:point val="3.2,9.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,9.195;5.905,9.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,9.2"/>
        <dia:point val="5.9,9.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O76">
      <dia:attribute name="obj_pos">
        <dia:point val="3.2,16.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,16.195;5.905,16.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,16.2"/>
        <dia:point val="5.9,16.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O77">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,16.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,16.695;5.905,16.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,16.7"/>
        <dia:point val="5.9,16.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O78">
      <dia:attribute name="obj_pos">
        <dia:point val="3.2,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,15.695;5.905,15.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,15.7"/>
        <dia:point val="5.9,15.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O79">
      <dia:attribute name="obj_pos">
        <dia:point val="3.2,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,15.195;5.905,15.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,15.2"/>
        <dia:point val="5.9,15.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O80">
      <dia:attribute name="obj_pos">
        <dia:point val="3.2,14.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,14.195;5.905,14.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,14.2"/>
        <dia:point val="5.9,14.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O81">
      <dia:attribute name="obj_pos">
        <dia:point val="3.2,14.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.195,14.695;5.905,14.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.2,14.7"/>
        <dia:point val="5.9,14.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O82">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,4.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,4.195;2.205,17.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,4.2"/>
        <dia:point val="2.2,17.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O83">
      <dia:attribute name="obj_pos">
        <dia:point val="1.7,17.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.695,17.195;5.905,17.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.7,17.2"/>
        <dia:point val="5.9,17.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O84">
      <dia:attribute name="obj_pos">
        <dia:point val="19.3,3.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="19.295,3.395;19.305,17.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="19.3,3.4"/>
        <dia:point val="19.3,17.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O85">
      <dia:attribute name="obj_pos">
        <dia:point val="15.7,3.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.695,3.395;15.705,17.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.7,3.4"/>
        <dia:point val="15.7,17.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O86">
      <dia:attribute name="obj_pos">
        <dia:point val="11.9,3.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.895,3.395;11.905,17.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.9,3.4"/>
        <dia:point val="11.9,17.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O87">
      <dia:attribute name="obj_pos">
        <dia:point val="8.9,3.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,3.395;8.905,17.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,3.4"/>
        <dia:point val="8.9,17.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O88">
      <dia:attribute name="obj_pos">
        <dia:point val="5.9,3.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.895,3.395;5.905,17.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.9,3.4"/>
        <dia:point val="5.9,17.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O89">
      <dia:attribute name="obj_pos">
        <dia:point val="11.9,5.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.895,5.195;15.705,5.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.9,5.7"/>
        <dia:point val="15.7,5.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O90">
      <dia:attribute name="obj_pos">
        <dia:point val="11.9,7.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.895,6.695;15.705,7.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.9,7.2"/>
        <dia:point val="15.7,6.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O91">
      <dia:attribute name="obj_pos">
        <dia:point val="8.9,17.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,16.695;11.905,17.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,17.2"/>
        <dia:point val="11.9,16.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O92">
      <dia:attribute name="obj_pos">
        <dia:point val="8.9,15.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,15.195;11.905,15.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,15.7"/>
        <dia:point val="11.9,15.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O93">
      <dia:attribute name="obj_pos">
        <dia:point val="8.9,16.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,15.695;11.905,16.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,16.2"/>
        <dia:point val="11.9,15.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O94">
      <dia:attribute name="obj_pos">
        <dia:point val="15.7,17.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.695,16.695;19.305,17.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.7,17.2"/>
        <dia:point val="19.3,16.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O95">
      <dia:attribute name="obj_pos">
        <dia:point val="1.7,18.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.695,18.495;19.305,18.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.7,18.5"/>
        <dia:point val="19.3,18.5"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O96">
      <dia:attribute name="obj_pos">
        <dia:point val="1.7,3.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.695,3.395;5.905,3.405"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.7,3.4"/>
        <dia:point val="5.9,3.4"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O97">
      <dia:attribute name="obj_pos">
        <dia:point val="8.9,7.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,7.195;11.905,7.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,7.7"/>
        <dia:point val="11.9,7.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O98">
      <dia:attribute name="obj_pos">
        <dia:point val="8.9,7.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,6.695;11.905,7.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,7.2"/>
        <dia:point val="11.9,6.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O99">
      <dia:attribute name="obj_pos">
        <dia:point val="8.9,5.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.895,5.195;11.905,5.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.9,5.7"/>
        <dia:point val="11.9,5.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O100">
      <dia:attribute name="obj_pos">
        <dia:point val="11.9,7.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.895,7.195;15.705,7.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.9,7.7"/>
        <dia:point val="15.7,7.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O101">
      <dia:attribute name="obj_pos">
        <dia:point val="15.7,7.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.695,7.195;19.305,7.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.7,7.7"/>
        <dia:point val="19.3,7.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O102">
      <dia:attribute name="obj_pos">
        <dia:point val="15.7,7.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.695,6.695;19.305,7.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.7,7.2"/>
        <dia:point val="19.3,6.7"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O103">
      <dia:attribute name="obj_pos">
        <dia:point val="15.7,5.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.695,5.195;19.305,5.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="15.7,5.7"/>
        <dia:point val="19.3,5.2"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O104">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,19"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,18.995;18.205,19.005"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,19"/>
        <dia:point val="18.2,19"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O105">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,19"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,18.995;2.205,24.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,19"/>
        <dia:point val="2.2,24.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O106">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,19.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,19.795;18.205,19.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,19.8"/>
        <dia:point val="18.2,19.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O107">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,20.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,20.295;18.205,20.305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,20.3"/>
        <dia:point val="18.2,20.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O108">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,20.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,20.795;18.205,20.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,20.8"/>
        <dia:point val="18.2,20.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O109">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,21.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,21.295;18.205,21.305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,21.3"/>
        <dia:point val="18.2,21.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O110">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,21.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,21.795;18.205,21.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,21.8"/>
        <dia:point val="18.2,21.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O111">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,22.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,22.295;18.205,22.305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,22.3"/>
        <dia:point val="18.2,22.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O112">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,22.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,22.795;18.205,22.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,22.8"/>
        <dia:point val="18.2,22.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O113">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,23.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,23.295;18.205,23.305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,23.3"/>
        <dia:point val="18.2,23.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O114">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,23.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,23.795;18.205,23.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,23.8"/>
        <dia:point val="18.2,23.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O115">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,24.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,24.295;18.205,24.305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,24.3"/>
        <dia:point val="18.2,24.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O116">
      <dia:attribute name="obj_pos">
        <dia:point val="4.5,19"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.495,18.995;4.505,24.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.5,19"/>
        <dia:point val="4.5,24.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O117">
      <dia:attribute name="obj_pos">
        <dia:point val="6,19"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.995,18.995;6.005,24.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="6,19"/>
        <dia:point val="6,24.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O118">
      <dia:attribute name="obj_pos">
        <dia:point val="9.35,19"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.345,18.995;9.355,24.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="9.35,19"/>
        <dia:point val="9.35,24.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O119">
      <dia:attribute name="obj_pos">
        <dia:point val="11.35,19"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.345,18.995;11.355,24.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.35,19"/>
        <dia:point val="11.35,24.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O120">
      <dia:attribute name="obj_pos">
        <dia:point val="7.5,19"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.495,18.995;7.505,24.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="7.5,19"/>
        <dia:point val="7.5,24.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O121">
      <dia:attribute name="obj_pos">
        <dia:point val="12.85,19"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.845,18.995;12.855,24.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.85,19"/>
        <dia:point val="12.85,24.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O122">
      <dia:attribute name="obj_pos">
        <dia:point val="16.2,19"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.195,18.995;16.205,24.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="16.2,19"/>
        <dia:point val="16.2,24.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O123">
      <dia:attribute name="obj_pos">
        <dia:point val="14.35,19"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.345,18.995;14.355,24.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="14.35,19"/>
        <dia:point val="14.35,24.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O124">
      <dia:attribute name="obj_pos">
        <dia:point val="11.35,19.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.345,19.795;11.355,20.305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.35,19.8"/>
        <dia:point val="9.35,20.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O125">
      <dia:attribute name="obj_pos">
        <dia:point val="12.85,20.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.345,20.295;12.855,20.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.85,20.3"/>
        <dia:point val="11.35,20.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O126">
      <dia:attribute name="obj_pos">
        <dia:point val="14.35,20.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.845,20.295;14.355,20.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="14.35,20.3"/>
        <dia:point val="12.85,20.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O127">
      <dia:attribute name="obj_pos">
        <dia:point val="12.85,22.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.345,22.795;12.855,23.305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.85,22.8"/>
        <dia:point val="11.35,23.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O128">
      <dia:attribute name="obj_pos">
        <dia:point val="14.35,22.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.845,22.795;14.355,23.305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="14.35,22.8"/>
        <dia:point val="12.85,23.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O129">
      <dia:attribute name="obj_pos">
        <dia:point val="16.2,22.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.345,22.795;16.205,23.305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="16.2,22.8"/>
        <dia:point val="14.35,23.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O130">
      <dia:attribute name="obj_pos">
        <dia:point val="18.2,22.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.195,22.795;18.205,23.305"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="18.2,22.8"/>
        <dia:point val="16.2,23.3"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O131">
      <dia:attribute name="obj_pos">
        <dia:point val="18.2,20.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.195,20.295;18.205,20.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="18.2,20.3"/>
        <dia:point val="16.2,20.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O132">
      <dia:attribute name="obj_pos">
        <dia:point val="18.2,19"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.195,18.995;18.205,24.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="18.2,19"/>
        <dia:point val="18.2,24.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O133">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,24.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.195,24.795;18.205,24.805"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.2,24.8"/>
        <dia:point val="18.2,24.8"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O134">
      <dia:attribute name="obj_pos">
        <dia:point val="2.85,26.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.84,26.54;2.86,27.76"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.85,26.55"/>
        <dia:point val="2.85,27.75"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O135">
      <dia:attribute name="obj_pos">
        <dia:point val="7.3,26.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.84,26.94;7.31,26.96"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="7.3,26.95"/>
        <dia:point val="2.85,26.95"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O136">
      <dia:attribute name="obj_pos">
        <dia:point val="4.85,26.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.84,26.54;4.86,27.76"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.85,26.55"/>
        <dia:point val="4.85,27.75"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O137">
      <dia:attribute name="obj_pos">
        <dia:point val="7.3,27.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.89,27.34;7.31,27.36"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="7.3,27.35"/>
        <dia:point val="3.9,27.35"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.02"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O138">
      <dia:attribute name="obj_pos">
        <dia:point val="3.9,26.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.89,26.94;3.91,27.76"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.9,26.95"/>
        <dia:point val="3.9,27.75"/>
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
    <dia:object type="Standard - Text" version="0" id="O139">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,11.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.8,11.3811;2.0828,12.5011"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#公
費
負
担#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.8,11.6"/>
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
    <dia:object type="Standard - Text" version="0" id="O140">
      <dia:attribute name="obj_pos">
        <dia:point val="2.55,11.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.55,11.0811;2.8328,13.3211"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#公
費
と
公
費
の
併
用#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.55,11.3"/>
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
    <dia:object type="Standard - Text" version="0" id="O141">
      <dia:attribute name="obj_pos">
        <dia:point val="2.55,14.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.55,14.6811;2.8328,15.8011"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#公
費
単
独#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.55,14.9"/>
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
    <dia:object type="Standard - Text" version="0" id="O142">
      <dia:attribute name="obj_pos">
        <dia:point val="2.85,17.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.85,16.8311;3.6984,17.1111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#合　計#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.85,17.05"/>
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
    <dia:object type="Standard - Text" version="0" id="O143">
      <dia:attribute name="obj_pos">
        <dia:point val="2.85,17.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.85,17.3311;4.8296,17.6111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#総件数　＋　＋#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.85,17.55"/>
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
    <dia:object type="Standard - Text" version="0" id="O144">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,18.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,18.5655;3.109,18.8655"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#備　考#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,18.8"/>
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
    <dia:object type="Standard - Text" version="0" id="O145">
      <dia:attribute name="obj_pos">
        <dia:point val="11.5,17.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.5,17.3311;11.7828,17.6111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.5,17.55"/>
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
    <dia:object type="Standard - Text" version="0" id="O146">
      <dia:attribute name="obj_pos">
        <dia:point val="18.975,17.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.975,17.3311;19.2578,17.6111"/>
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
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.975,17.55"/>
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
    <dia:object type="Standard - Text" version="0" id="O147">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,17.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.2344,17.3311;14.3656,17.6111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#請求金額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,17.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
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
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O148">
        <dia:attribute name="obj_pos">
          <dia:point val="2.35849,17.0478"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.35849,16.8523;2.61099,17.1023"/>
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
              <dia:real val="0.312500"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.35849,17.0478"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
            <dia:attribute name="single">
              <dia:real val="0.1515"/>
            </dia:attribute>
            <dia:attribute name="multi">
              <dia:real val="0.12625"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Ellipse" version="0" id="O149">
        <dia:attribute name="obj_pos">
          <dia:point val="2.35,16.8219"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.345,16.8169;2.605,17.0769"/>
        </dia:attribute>
        <dia:attribute name="elem_corner">
          <dia:point val="2.35,16.8219"/>
        </dia:attribute>
        <dia:attribute name="elem_width">
          <dia:real val="0.25"/>
        </dia:attribute>
        <dia:attribute name="elem_height">
          <dia:real val="0.25"/>
        </dia:attribute>
        <dia:attribute name="border_width">
          <dia:real val="0.01"/>
        </dia:attribute>
        <dia:attribute name="show_background">
          <dia:boolean val="false"/>
        </dia:attribute>
      </dia:object>
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Ellipse" version="0" id="O150">
        <dia:attribute name="obj_pos">
          <dia:point val="2.32812,7.33125"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.32312,7.32625;2.58312,7.58625"/>
        </dia:attribute>
        <dia:attribute name="elem_corner">
          <dia:point val="2.32812,7.33125"/>
        </dia:attribute>
        <dia:attribute name="elem_width">
          <dia:real val="0.25"/>
        </dia:attribute>
        <dia:attribute name="elem_height">
          <dia:real val="0.25"/>
        </dia:attribute>
        <dia:attribute name="border_width">
          <dia:real val="0.01"/>
        </dia:attribute>
        <dia:attribute name="show_background">
          <dia:boolean val="false"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="0" id="O151">
        <dia:attribute name="obj_pos">
          <dia:point val="2.33661,7.55714"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="2.33661,7.36169;2.58911,7.61169"/>
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
              <dia:real val="0.312500"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="2.33661,7.55714"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
            <dia:attribute name="single">
              <dia:real val="0.1515"/>
            </dia:attribute>
            <dia:attribute name="multi">
              <dia:real val="0.12625"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
      </dia:object>
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Ellipse" version="0" id="O152">
        <dia:attribute name="obj_pos">
          <dia:point val="4.25,17.325"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.245,17.32;4.505,17.58"/>
        </dia:attribute>
        <dia:attribute name="elem_corner">
          <dia:point val="4.25,17.325"/>
        </dia:attribute>
        <dia:attribute name="elem_width">
          <dia:real val="0.25"/>
        </dia:attribute>
        <dia:attribute name="elem_height">
          <dia:real val="0.25"/>
        </dia:attribute>
        <dia:attribute name="border_width">
          <dia:real val="0.01"/>
        </dia:attribute>
        <dia:attribute name="show_background">
          <dia:boolean val="false"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="0" id="O153">
        <dia:attribute name="obj_pos">
          <dia:point val="4.25849,17.5508"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.25849,17.3553;4.51099,17.6053"/>
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
              <dia:real val="0.312500"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.25849,17.5508"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
            <dia:attribute name="single">
              <dia:real val="0.1515"/>
            </dia:attribute>
            <dia:attribute name="multi">
              <dia:real val="0.12625"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
      </dia:object>
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Ellipse" version="0" id="O154">
        <dia:attribute name="obj_pos">
          <dia:point val="3.7,17.325"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3.695,17.32;3.955,17.58"/>
        </dia:attribute>
        <dia:attribute name="elem_corner">
          <dia:point val="3.7,17.325"/>
        </dia:attribute>
        <dia:attribute name="elem_width">
          <dia:real val="0.25"/>
        </dia:attribute>
        <dia:attribute name="elem_height">
          <dia:real val="0.25"/>
        </dia:attribute>
        <dia:attribute name="border_width">
          <dia:real val="0.01"/>
        </dia:attribute>
        <dia:attribute name="show_background">
          <dia:boolean val="false"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="0" id="O155">
        <dia:attribute name="obj_pos">
          <dia:point val="3.70849,17.5508"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3.70849,17.3553;3.96099,17.6053"/>
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
              <dia:real val="0.312500"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="3.70849,17.5508"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
            <dia:attribute name="single">
              <dia:real val="0.1515"/>
            </dia:attribute>
            <dia:attribute name="multi">
              <dia:real val="0.12625"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
      </dia:object>
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Ellipse" version="0" id="O156">
        <dia:attribute name="obj_pos">
          <dia:point val="4.8,17.325"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.795,17.32;5.055,17.58"/>
        </dia:attribute>
        <dia:attribute name="elem_corner">
          <dia:point val="4.8,17.325"/>
        </dia:attribute>
        <dia:attribute name="elem_width">
          <dia:real val="0.25"/>
        </dia:attribute>
        <dia:attribute name="elem_height">
          <dia:real val="0.25"/>
        </dia:attribute>
        <dia:attribute name="border_width">
          <dia:real val="0.01"/>
        </dia:attribute>
        <dia:attribute name="show_background">
          <dia:boolean val="false"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="0" id="O157">
        <dia:attribute name="obj_pos">
          <dia:point val="4.80849,17.5508"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.80849,17.3553;5.06099,17.6053"/>
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
              <dia:real val="0.312500"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.80849,17.5508"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
            </dia:attribute>
            <dia:attribute name="alignment">
              <dia:enum val="0"/>
            </dia:attribute>
            <dia:attribute name="single">
              <dia:real val="0.1515"/>
            </dia:attribute>
            <dia:attribute name="multi">
              <dia:real val="0.12625"/>
            </dia:attribute>
          </dia:composite>
        </dia:attribute>
      </dia:object>
    </dia:group>
    <dia:object type="Standard - Text" version="0" id="O158">
      <dia:attribute name="obj_pos">
        <dia:point val="2.35,8.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.35,8.13109;2.6328,10.0911"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#　
老
人
　
の
併
用#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.35,8.35"/>
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
    <dia:object type="Standard - Text" version="0" id="O159">
      <dia:attribute name="obj_pos">
        <dia:point val="2.8,8.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.8,8.48109;3.0828,9.88109"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#公
費
と
医
保#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.8,8.7"/>
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
    <dia:object type="Standard - Arc" version="0" id="O160">
      <dia:attribute name="obj_pos">
        <dia:point val="2.35,9.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.35,9.05;2.65,9.15"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.35,9.05"/>
        <dia:point val="2.65,9.05"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Arc" version="0" id="O161">
      <dia:attribute name="obj_pos">
        <dia:point val="2.35,8.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.35,8.25;2.65,8.35"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.35,8.35"/>
        <dia:point val="2.65,8.35"/>
      </dia:attribute>
      <dia:attribute name="curve_distance">
        <dia:real val="-0.1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O162">
      <dia:attribute name="obj_pos">
        <dia:point val="2.2,28.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.2,28.0811;9.27,28.3611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#備考　この用紙は、日本工業規格Ａ列４番とすること。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.2,28.3"/>
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
    <dia:object type="Standard - Text" version="0" id="O163">
      <dia:attribute name="obj_pos">
        <dia:point val="7.4,3.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.1172,3.68109;7.6828,3.96109"/>
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
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.4,3.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
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
    <dia:object type="Standard - Text" version="0" id="O164">
      <dia:attribute name="obj_pos">
        <dia:point val="10.4,3.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.693,3.68109;11.107,3.96109"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#診療実日数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.4,3.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
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
    <dia:object type="Standard - Text" version="0" id="O165">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,3.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.5172,3.68109;14.0828,3.96109"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,3.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
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
    <dia:object type="Standard - Text" version="0" id="O166">
      <dia:attribute name="obj_pos">
        <dia:point val="17.5,3.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.086,3.68109;18.914,3.96109"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#一部負担金（控除額）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.5,3.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
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
    <dia:object type="Standard - Text" version="0" id="O167">
      <dia:attribute name="obj_pos">
        <dia:point val="3.8,3.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.2344,3.68109;4.3656,3.96109"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#区　　分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.8,3.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
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
    <dia:object type="Standard - Text" version="0" id="O168">
      <dia:attribute name="obj_pos">
        <dia:point val="13.5,2.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.5,1.98109;15.4796,2.26109"/>
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
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.5,2.2"/>
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
    <dia:object type="Standard - Text" version="0" id="O169">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,5.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.8,5.43109;2.0828,6.55109"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#老
人
保
健#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.8,5.65"/>
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
    <dia:object type="Standard - Text" version="0" id="O170">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,5.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,4.83109;4.4312,5.11109"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#老人単独#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,5.05"/>
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
    <dia:object type="Standard - Text" version="0" id="O171">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,6.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,6.33109;4.4312,6.61109"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#老人単独#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,6.55"/>
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
    <dia:object type="Standard - Text" version="0" id="O172">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,5.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,5.33109;4.1484,5.61109"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#小　計#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,5.55"/>
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
    <dia:object type="Standard - Text" version="0" id="O173">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,7.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,6.83109;4.1484,7.11109"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#小　計#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,7.05"/>
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
    <dia:object type="Standard - Text" version="0" id="O174">
      <dia:attribute name="obj_pos">
        <dia:point val="2.83125,7.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.83125,7.33109;3.67965,7.61109"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#合　計#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.83125,7.55"/>
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
    <dia:object type="Standard - Text" version="0" id="O175">
      <dia:attribute name="obj_pos">
        <dia:point val="2.55,4.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.55,4.43109;2.8328,5.55109"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#老
人
九
割#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.55,4.65"/>
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
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O176">
        <dia:attribute name="obj_pos">
          <dia:point val="3.3,4.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3.3,4.33109;3.5828,4.61109"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#老#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="3.3,4.55"/>
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
      <dia:object type="Standard - Text" version="0" id="O177">
        <dia:attribute name="obj_pos">
          <dia:point val="5.55,4.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="5.55,4.33109;5.8328,4.61109"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#用#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="5.55,4.55"/>
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
      <dia:object type="Standard - Text" version="0" id="O178">
        <dia:attribute name="obj_pos">
          <dia:point val="3.55,4.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3.55,4.33109;3.8328,4.61109"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#人#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="3.55,4.55"/>
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
      <dia:object type="Standard - Text" version="0" id="O179">
        <dia:attribute name="obj_pos">
          <dia:point val="4.05,4.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.05,4.33109;4.3328,4.61109"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#割#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.05,4.55"/>
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
      <dia:object type="Standard - Text" version="0" id="O180">
        <dia:attribute name="obj_pos">
          <dia:point val="3.8,4.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3.8,4.33109;4.0828,4.61109"/>
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
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="3.8,4.55"/>
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
      <dia:object type="Standard - Text" version="0" id="O181">
        <dia:attribute name="obj_pos">
          <dia:point val="4.3,4.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.3,4.33109;4.5828,4.61109"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#と#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.3,4.55"/>
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
      <dia:object type="Standard - Text" version="0" id="O182">
        <dia:attribute name="obj_pos">
          <dia:point val="4.55,4.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.55,4.33109;4.8328,4.61109"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#公#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.55,4.55"/>
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
      <dia:object type="Standard - Text" version="0" id="O183">
        <dia:attribute name="obj_pos">
          <dia:point val="4.8,4.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.8,4.33109;5.0828,4.61109"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#費#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.8,4.55"/>
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
      <dia:object type="Standard - Text" version="0" id="O184">
        <dia:attribute name="obj_pos">
          <dia:point val="5.05,4.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="5.05,4.33109;5.3328,4.61109"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#の#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="5.05,4.55"/>
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
      <dia:object type="Standard - Text" version="0" id="O185">
        <dia:attribute name="obj_pos">
          <dia:point val="5.3,4.55"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="5.3,4.33109;5.5828,4.61109"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#併#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="5.3,4.55"/>
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
    <dia:object type="Standard - Text" version="0" id="O186">
      <dia:attribute name="obj_pos">
        <dia:point val="2.55,6.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.55,5.93109;2.8328,7.05109"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#老
人
七
割#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.55,6.15"/>
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
    <dia:object type="Standard - Text" version="0" id="O187">
      <dia:attribute name="obj_pos">
        <dia:point val="3.35,19.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.643,19.2811;4.057,19.5611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#区　　　分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.35,19.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
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
    <dia:object type="Standard - Text" version="0" id="O188">
      <dia:attribute name="obj_pos">
        <dia:point val="5.25,19.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8258,19.2811;5.6742,19.5611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件　数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.25,19.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
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
    <dia:object type="Standard - Text" version="0" id="O189">
      <dia:attribute name="obj_pos">
        <dia:point val="6.75,19.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3258,19.2811;7.1742,19.5611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#日　数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.75,19.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
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
    <dia:object type="Standard - Text" version="0" id="O190">
      <dia:attribute name="obj_pos">
        <dia:point val="8.4,19.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.693,19.2811;9.107,19.5611"/>
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
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.4,19.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
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
    <dia:object type="Standard - Text" version="0" id="O191">
      <dia:attribute name="obj_pos">
        <dia:point val="10.35,19.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.643,19.2811;11.057,19.5611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#控　除　額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.35,19.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
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
    <dia:object type="Standard - Text" version="0" id="O192">
      <dia:attribute name="obj_pos">
        <dia:point val="12.1,19.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.5344,19.2811;12.6656,19.5611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#食事件数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.1,19.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
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
    <dia:object type="Standard - Text" version="0" id="O193">
      <dia:attribute name="obj_pos">
        <dia:point val="13.6,19.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.0344,19.2811;14.1656,19.5611"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#食事回数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.6,19.5"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
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
    <dia:object type="Standard - Text" version="0" id="O194">
      <dia:attribute name="obj_pos">
        <dia:point val="15.25,19.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.6844,19.1311;15.8156,19.6911"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#食事療養
の金額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.25,19.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
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
    <dia:object type="Standard - Text" version="0" id="O195">
      <dia:attribute name="obj_pos">
        <dia:point val="17.2,19.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.493,19.1311;17.907,19.6911"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#標準負担額
（公費分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="17.2,19.35"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="1"/>
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
    <dia:object type="Standard - Text" version="0" id="O196">
      <dia:attribute name="obj_pos">
        <dia:point val="2.3,20.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.3,19.9311;4.5624,20.2111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#４３（老人被爆）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.3,20.15"/>
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
    <dia:object type="Standard - Text" version="0" id="O197">
      <dia:attribute name="obj_pos">
        <dia:point val="2.3,20.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.3,20.4311;4.50584,20.7111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１０（感37の2）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.3,20.65"/>
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
    <dia:object type="Standard - Text" version="0" id="O198">
      <dia:attribute name="obj_pos">
        <dia:point val="2.3,21.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.3,20.9311;4.5624,21.2111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１１（結核入院）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.3,21.15"/>
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
    <dia:object type="Standard - Text" version="0" id="O199">
      <dia:attribute name="obj_pos">
        <dia:point val="2.3,21.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.3,21.4311;3.9968,21.7111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１２（生保）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.3,21.65"/>
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
    <dia:object type="Standard - Text" version="0" id="O200">
      <dia:attribute name="obj_pos">
        <dia:point val="2.3,22.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.3,21.9311;3.9968,22.2111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１５（更生）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.3,22.15"/>
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
    <dia:object type="Standard - Text" version="0" id="O201">
      <dia:attribute name="obj_pos">
        <dia:point val="2.3,22.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.3,22.4311;4.2796,22.7111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２０（精２９）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.3,22.65"/>
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
    <dia:object type="Standard - Text" version="0" id="O202">
      <dia:attribute name="obj_pos">
        <dia:point val="2.3,23.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.3,22.9311;4.5624,23.2111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２１（精神通院）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.3,23.15"/>
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
    <dia:object type="Standard - Text" version="0" id="O203">
      <dia:attribute name="obj_pos">
        <dia:point val="2.3,23.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.3,23.4311;3.9968,23.7111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２２（麻薬）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.3,23.65"/>
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
    <dia:object type="Standard - Text" version="0" id="O204">
      <dia:attribute name="obj_pos">
        <dia:point val="2.3,24.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.3,23.9311;4.2796,24.2111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#２８（感染症）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.3,24.15"/>
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
    <dia:object type="Standard - Text" version="0" id="O205">
      <dia:attribute name="obj_pos">
        <dia:point val="2.3,24.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.3,24.4311;3.9968,24.7111"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#５１（特疾）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.3,24.65"/>
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
    <dia:object type="Standard - Text" version="0" id="O206">
      <dia:attribute name="obj_pos">
        <dia:point val="1.75,27.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.75,26.8967;2.8004,27.4167"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#１件当り
平均点数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.325000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.75,27.1"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.15756"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1313"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O207">
      <dia:attribute name="obj_pos">
        <dia:point val="3.1,27.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.1,27.2155;3.706,27.5155"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#医保#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.1,27.45"/>
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
    <dia:object type="Standard - Text" version="0" id="O208">
      <dia:attribute name="obj_pos">
        <dia:point val="4.1,27.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.1,27.0155;4.706,27.3155"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#本人#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.1,27.25"/>
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
    <dia:object type="Standard - Text" version="0" id="O209">
      <dia:attribute name="obj_pos">
        <dia:point val="4.1,27.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.1,27.4155;4.706,27.7155"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#家族#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.1,27.65"/>
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
    <dia:object type="Standard - Text" version="0" id="O210">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,26.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,26.6155;4.512,26.9155"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#老人保健#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,26.85"/>
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
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O211">
        <dia:attribute name="obj_pos">
          <dia:point val="3.3,6.05"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3.3,5.83109;3.5828,6.11109"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#老#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="3.3,6.05"/>
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
      <dia:object type="Standard - Text" version="0" id="O212">
        <dia:attribute name="obj_pos">
          <dia:point val="5.55,6.05"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="5.55,5.83109;5.8328,6.11109"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#用#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="5.55,6.05"/>
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
      <dia:object type="Standard - Text" version="0" id="O213">
        <dia:attribute name="obj_pos">
          <dia:point val="3.55,6.05"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3.55,5.83109;3.8328,6.11109"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#人#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="3.55,6.05"/>
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
      <dia:object type="Standard - Text" version="0" id="O214">
        <dia:attribute name="obj_pos">
          <dia:point val="4.05,6.05"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.05,5.83109;4.3328,6.11109"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#割#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.05,6.05"/>
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
      <dia:object type="Standard - Text" version="0" id="O215">
        <dia:attribute name="obj_pos">
          <dia:point val="3.8,6.05"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="3.8,5.83109;4.0828,6.11109"/>
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
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="3.8,6.05"/>
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
      <dia:object type="Standard - Text" version="0" id="O216">
        <dia:attribute name="obj_pos">
          <dia:point val="4.3,6.05"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.3,5.83109;4.5828,6.11109"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#と#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.3,6.05"/>
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
      <dia:object type="Standard - Text" version="0" id="O217">
        <dia:attribute name="obj_pos">
          <dia:point val="4.55,6.05"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.55,5.83109;4.8328,6.11109"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#公#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.55,6.05"/>
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
      <dia:object type="Standard - Text" version="0" id="O218">
        <dia:attribute name="obj_pos">
          <dia:point val="4.8,6.05"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.8,5.83109;5.0828,6.11109"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#費#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.8,6.05"/>
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
      <dia:object type="Standard - Text" version="0" id="O219">
        <dia:attribute name="obj_pos">
          <dia:point val="5.05,6.05"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="5.05,5.83109;5.3328,6.11109"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#の#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="5.05,6.05"/>
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
      <dia:object type="Standard - Text" version="0" id="O220">
        <dia:attribute name="obj_pos">
          <dia:point val="5.3,6.05"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="5.3,5.83109;5.5828,6.11109"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#併#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.350000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="5.3,6.05"/>
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
  </dia:layer>
  <dia:layer name="差し込み" visible="true">
    <dia:object type="Embed - Text" version="0" id="O221">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,4.55005"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.941,4.3155;8.85,4.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[0].ROU-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,4.55005"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[0].ROU-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O222">
      <dia:attribute name="obj_pos">
        <dia:point val="11.85,4.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.941,4.31545;11.85,4.61545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[0].ROU-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#NISSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.85,4.55"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[0].ROU-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O223">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,4.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.741,4.31545;15.65,4.61545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[0].ROU-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,4.55"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[0].ROU-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O224">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,4.55005"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.1592,4.3155;19.25,4.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[0].ROU-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,4.55005"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[0].ROU-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O225">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,5.05005"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.941,4.8155;8.85,5.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[1].ROU-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,5.05005"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[1].ROU-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O226">
      <dia:attribute name="obj_pos">
        <dia:point val="11.85,5.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.941,4.81545;11.85,5.11545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[1].ROU-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#NISSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.85,5.05"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[1].ROU-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O227">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,5.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.741,4.81545;15.65,5.11545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[1].ROU-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,5.05"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[1].ROU-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O228">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,5.05005"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.1592,4.8155;19.25,5.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[1].ROU-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,5.05005"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[1].ROU-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O229">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,5.55005"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.941,5.3155;8.85,5.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[2].ROU-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,5.55005"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[2].ROU-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O230">
      <dia:attribute name="obj_pos">
        <dia:point val="11.85,5.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.941,5.31545;11.85,5.61545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[2].ROU-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#NISSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.85,5.55"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[2].ROU-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O231">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,5.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.741,5.31545;15.65,5.61545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[2].ROU-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,5.55"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[2].ROU-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O232">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,5.55005"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.1592,5.3155;19.25,5.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[2].ROU-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,5.55005"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[0].ROU-G[2].ROU-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O233">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,2.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.65,1.96545;16.7408,2.26545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#HOSPCD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#HOSPCD#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,2.2"/>
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
        <dia:string>#HOSPCD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O234">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4,17.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.3092,17.3155;11.4,17.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#GKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#GKENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4,17.55"/>
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
        <dia:string>#GKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O235">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,6.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.941,5.81545;8.85,6.11545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[0].ROU-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,6.05"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[0].ROU-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O236">
      <dia:attribute name="obj_pos">
        <dia:point val="11.85,6.04995"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.941,5.8154;11.85,6.1154"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[0].ROU-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#NISSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.85,6.04995"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[0].ROU-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O237">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,6.04995"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.741,5.8154;15.65,6.1154"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[0].ROU-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,6.04995"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[0].ROU-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O238">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,6.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.1592,5.81545;19.25,6.11545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[0].ROU-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,6.05"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[0].ROU-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O239">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,6.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.941,6.31545;8.85,6.61545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[1].ROU-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,6.55"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[1].ROU-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O240">
      <dia:attribute name="obj_pos">
        <dia:point val="11.85,6.54995"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.941,6.3154;11.85,6.6154"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[1].ROU-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#NISSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.85,6.54995"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[1].ROU-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O241">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,6.54995"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.741,6.3154;15.65,6.6154"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[1].ROU-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,6.54995"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[1].ROU-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O242">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,6.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.1592,6.31545;19.25,6.61545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[1].ROU-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,6.55"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[1].ROU-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O243">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,7.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.941,6.81545;8.85,7.11545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[2].ROU-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,7.05"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[2].ROU-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O244">
      <dia:attribute name="obj_pos">
        <dia:point val="11.85,7.04995"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.941,6.8154;11.85,7.1154"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[2].ROU-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#NISSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.85,7.04995"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[2].ROU-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O245">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,7.04995"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.741,6.8154;15.65,7.1154"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[2].ROU-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,7.04995"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[2].ROU-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O246">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,7.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.1592,6.81545;19.25,7.11545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[2].ROU-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,7.05"/>
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
        <dia:string>#ROU-TBL.ROU-TBL1[1].ROU-G[2].ROU-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O247">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,8.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2138,7.81545;8.85,8.11545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[0].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,8.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[0].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O248">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,8.04995"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0138,7.8154;15.65,8.1154"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[0].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,8.04995"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[0].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O249">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,8.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.432,7.81545;19.25,8.11545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[0].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,8.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[0].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O250">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,8.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2138,8.31545;8.85,8.61545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[1].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,8.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[1].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O251">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,8.54995"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0138,8.3154;15.65,8.6154"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[1].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,8.54995"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[1].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O252">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,8.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.432,8.31545;19.25,8.61545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[1].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,8.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[1].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O253">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,9.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2138,8.81545;8.85,9.11545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[2].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,9.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[2].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O254">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,9.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0138,8.81545;15.65,9.11545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[2].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,9.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[2].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O255">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,9.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.432,8.81545;19.25,9.11545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[2].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,9.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[2].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O256">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,9.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2138,9.31545;8.85,9.61545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[3].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,9.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[3].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O257">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,9.54995"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0138,9.3154;15.65,9.6154"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[3].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,9.54995"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[3].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O258">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,9.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.432,9.31545;19.25,9.61545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[3].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,9.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[3].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O259">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,10.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2138,9.81545;8.85,10.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[4].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,10.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[4].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O260">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,10.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0138,9.81545;15.65,10.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[4].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,10.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[4].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O261">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,10.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.432,9.81545;19.25,10.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[4].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,10.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[4].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O262">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,10.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2138,10.3155;8.85,10.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[5].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,10.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[5].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O263">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,10.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0138,10.3155;15.65,10.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[5].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,10.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[5].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O264">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,10.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.432,10.3155;19.25,10.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[5].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,10.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[5].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O265">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,11.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2138,10.8155;8.85,11.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[0].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,11.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[0].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O266">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,11.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0138,10.8155;15.65,11.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[0].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,11.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[0].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O267">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,11.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.432,10.8155;19.25,11.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[0].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,11.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[0].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O268">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,11.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2138,11.3155;8.85,11.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[1].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,11.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[1].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O269">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,11.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0138,11.3155;15.65,11.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[1].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,11.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[1].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O270">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,11.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.432,11.3155;19.25,11.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[1].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,11.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[1].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O271">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,12.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2138,11.8155;8.85,12.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[2].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,12.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[2].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O272">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,12.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0138,11.8155;15.65,12.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[2].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,12.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[2].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O273">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,12.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.432,11.8155;19.25,12.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[2].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,12.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[2].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O274">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,12.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2138,12.3155;8.85,12.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[3].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,12.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[3].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O275">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,12.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0138,12.3155;15.65,12.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[3].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,12.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[3].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O276">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,12.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.432,12.3155;19.25,12.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[3].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,12.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[3].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O277">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,13.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2138,12.8155;8.85,13.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[4].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,13.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[4].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O278">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,13.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0138,12.8155;15.65,13.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[4].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,13.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[4].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O279">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,13.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.432,12.8155;19.25,13.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[4].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,13.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[4].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O280">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,13.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2138,13.3155;8.85,13.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[5].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,13.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[5].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O281">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,13.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0138,13.3155;15.65,13.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[5].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,13.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[5].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O282">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,13.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.432,13.3155;19.25,13.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[5].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,13.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[5].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O283">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,14.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2138,13.8155;8.85,14.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[0].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,14.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[0].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O284">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,14.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0138,13.8155;15.65,14.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[0].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,14.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[0].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O285">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,14.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.432,13.8155;19.25,14.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[0].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,14.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[0].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O286">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,14.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2138,14.3155;8.85,14.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[1].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,14.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[1].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O287">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,14.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0138,14.3155;15.65,14.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[1].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,14.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[1].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O288">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,14.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.432,14.3155;19.25,14.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[1].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,14.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[1].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O289">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,15.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2138,14.8155;8.85,15.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[2].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,15.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[2].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O290">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,15.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0138,14.8155;15.65,15.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[2].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,15.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[2].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O291">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,15.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.432,14.8155;19.25,15.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[2].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,15.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[2].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O292">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,15.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2138,15.3155;8.85,15.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[3].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,15.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[3].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O293">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,15.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0138,15.3155;15.65,15.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[3].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,15.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[3].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O294">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,15.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.432,15.3155;19.25,15.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[3].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,15.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[3].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O295">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,16.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2138,15.8155;8.85,16.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[4].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,16.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[4].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O296">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,16.0499"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0138,15.8154;15.65,16.1154"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[4].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,16.0499"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[4].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O297">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,16.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.432,15.8155;19.25,16.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[4].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,16.05"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[4].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O298">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,16.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.2138,16.3155;8.85,16.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[5].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-KENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,16.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[5].KOH-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O299">
      <dia:attribute name="obj_pos">
        <dia:point val="15.65,16.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0138,16.3155;15.65,16.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[5].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-TENSU#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.65,16.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[5].KOH-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O300">
      <dia:attribute name="obj_pos">
        <dia:point val="19.25,16.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.432,16.3155;19.25,16.6155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[5].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KOH-ITBFTN#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.25,16.55"/>
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
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[5].KOH-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O301">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,7.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7592,7.31545;8.85,7.61545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KENSUG[0]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KENSUG#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,7.55"/>
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
        <dia:string>#KENSUG[0]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O302">
      <dia:attribute name="obj_pos">
        <dia:point val="8.85,17.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7592,16.8155;8.85,17.1155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KENSUG[1]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#KENSUG#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.85,17.05"/>
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
        <dia:string>#KENSUG[1]#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O303">
      <dia:attribute name="obj_pos">
        <dia:point val="7.5,2.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.5,2.46545;12.954,2.76545"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#TITLE#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.5,2.7"/>
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
        <dia:string>#TITLE#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="50"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O304">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,7.925"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,7.74518;4.9261,8.20518"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[0].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえおかき
くけこさしすせ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,7.925"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.13938"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.11615"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[0].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="44"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="11"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O305">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,8.425"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,8.24518;4.9261,8.70518"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[1].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえおかき
くけこさしすせ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,8.425"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.13938"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.11615"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[1].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="44"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="11"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O306">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,8.925"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,8.74518;4.9261,9.20518"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[2].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえおかき
くけこさしすせ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,8.925"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.13938"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.11615"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[2].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="44"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="11"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O307">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,9.425"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,9.24518;4.9261,9.70518"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[3].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえおかき
くけこさしすせ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,9.425"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.13938"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.11615"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[3].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="44"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="11"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O308">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,9.925"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,9.74518;4.9261,10.2052"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[4].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえおかき
くけこさしすせ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,9.925"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.13938"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.11615"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[4].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="44"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="11"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O309">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,10.425"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,10.2452;4.9261,10.7052"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[5].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえおかき
くけこさしすせ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,10.425"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.13938"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.11615"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL.KOH-TBL1[0].KOH-G[5].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="44"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="11"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O310">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,10.925"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,10.7452;4.9261,11.2052"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[0].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえおかき
くけこさしすせ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,10.925"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.13938"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.11615"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[0].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="44"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="11"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O311">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,11.425"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,11.2452;4.9261,11.7052"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[1].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえおかき
くけこさしすせ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,11.425"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.13938"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.11615"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[1].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="44"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="11"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O312">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,11.925"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,11.7452;4.9261,12.2052"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[2].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえおかき
くけこさしすせ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,11.925"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.13938"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.11615"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[2].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="44"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="11"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O313">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,12.425"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,12.2452;4.9261,12.7052"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[3].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえおかき
くけこさしすせ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,12.425"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.13938"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.11615"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[3].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="44"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="11"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O314">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,12.925"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,12.7452;4.9261,13.2052"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[4].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえおかき
くけこさしすせ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,12.925"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.13938"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.11615"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[4].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="44"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="11"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O315">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,13.425"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,13.2452;4.9261,13.7052"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[5].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえおかき
くけこさしすせ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,13.425"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.13938"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.11615"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL.KOH-TBL1[1].KOH-G[5].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="44"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="11"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O316">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,13.925"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,13.7452;4.9261,14.2052"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[0].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえおかき
くけこさしすせ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,13.925"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.13938"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.11615"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[0].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="44"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="11"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O317">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,14.425"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,14.2452;4.9261,14.7052"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[1].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえおかき
くけこさしすせ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,14.425"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.13938"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.11615"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[1].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="44"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="11"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O318">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,14.925"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,14.7452;4.9261,15.2052"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[2].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえおかき
くけこさしすせ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,14.925"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.13938"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.11615"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[2].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="44"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="11"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O319">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,15.425"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,15.2452;4.9261,15.7052"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[3].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえおかき
くけこさしすせ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,15.425"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.13938"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.11615"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[3].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="44"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="11"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O320">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,15.925"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,15.7452;4.9261,16.2052"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[4].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえおかき
くけこさしすせ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,15.925"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.13938"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.11615"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[4].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="44"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="11"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O321">
      <dia:attribute name="obj_pos">
        <dia:point val="3.3,16.425"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.3,16.2452;4.9261,16.7052"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[5].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_column">
        <dia:int val="22"/>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あいうえおかき
くけこさしすせ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.3,16.425"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
          <dia:attribute name="single">
            <dia:real val="0.13938"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.11615"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KOH-TBL.KOH-TBL1[2].KOH-G[5].KOHKBN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="44"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="11"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O322">
      <dia:attribute name="obj_pos">
        <dia:point val="5.9,20.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8092,19.9155;5.9,20.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[0].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.9,20.15"/>
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
        <dia:string>#BKO-TBL[0].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O323">
      <dia:attribute name="obj_pos">
        <dia:point val="7.4,20.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3092,19.9155;7.4,20.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[0].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.4,20.15"/>
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
        <dia:string>#BKO-TBL[0].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O324">
      <dia:attribute name="obj_pos">
        <dia:point val="9.25,20.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7956,19.9155;9.25,20.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[0].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.25,20.15"/>
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
        <dia:string>#BKO-TBL[0].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O325">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,20.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.6138,19.9155;11.25,20.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[0].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,20.15"/>
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
        <dia:string>#BKO-TBL[0].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O326">
      <dia:attribute name="obj_pos">
        <dia:point val="5.9,20.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8092,20.4155;5.9,20.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[1].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.9,20.65"/>
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
        <dia:string>#BKO-TBL[1].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O327">
      <dia:attribute name="obj_pos">
        <dia:point val="7.4,20.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3092,20.4155;7.4,20.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[1].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.4,20.65"/>
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
        <dia:string>#BKO-TBL[1].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O328">
      <dia:attribute name="obj_pos">
        <dia:point val="9.25,20.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7956,20.4155;9.25,20.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[1].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.25,20.65"/>
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
        <dia:string>#BKO-TBL[1].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O329">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,20.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.6138,20.4155;11.25,20.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[1].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,20.65"/>
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
        <dia:string>#BKO-TBL[1].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O330">
      <dia:attribute name="obj_pos">
        <dia:point val="5.9,21.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8092,20.9155;5.9,21.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[2].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.9,21.15"/>
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
        <dia:string>#BKO-TBL[2].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O331">
      <dia:attribute name="obj_pos">
        <dia:point val="7.4,21.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3092,20.9155;7.4,21.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[2].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.4,21.15"/>
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
        <dia:string>#BKO-TBL[2].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O332">
      <dia:attribute name="obj_pos">
        <dia:point val="9.25,21.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7956,20.9155;9.25,21.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[2].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.25,21.15"/>
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
        <dia:string>#BKO-TBL[2].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O333">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,21.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.6138,20.9155;11.25,21.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[2].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,21.15"/>
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
        <dia:string>#BKO-TBL[2].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O334">
      <dia:attribute name="obj_pos">
        <dia:point val="5.9,21.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8092,21.4155;5.9,21.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[3].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.9,21.65"/>
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
        <dia:string>#BKO-TBL[3].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O335">
      <dia:attribute name="obj_pos">
        <dia:point val="7.4,21.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3092,21.4155;7.4,21.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[3].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.4,21.65"/>
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
        <dia:string>#BKO-TBL[3].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O336">
      <dia:attribute name="obj_pos">
        <dia:point val="9.25,21.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7956,21.4155;9.25,21.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[3].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.25,21.65"/>
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
        <dia:string>#BKO-TBL[3].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O337">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,21.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.6138,21.4155;11.25,21.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[3].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,21.65"/>
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
        <dia:string>#BKO-TBL[3].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O338">
      <dia:attribute name="obj_pos">
        <dia:point val="5.9,22.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8092,21.9155;5.9,22.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[4].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.9,22.15"/>
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
        <dia:string>#BKO-TBL[4].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O339">
      <dia:attribute name="obj_pos">
        <dia:point val="7.4,22.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3092,21.9155;7.4,22.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[4].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.4,22.15"/>
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
        <dia:string>#BKO-TBL[4].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O340">
      <dia:attribute name="obj_pos">
        <dia:point val="9.25,22.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7956,21.9155;9.25,22.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[4].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.25,22.15"/>
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
        <dia:string>#BKO-TBL[4].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O341">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,22.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.6138,21.9155;11.25,22.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[4].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,22.15"/>
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
        <dia:string>#BKO-TBL[4].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O342">
      <dia:attribute name="obj_pos">
        <dia:point val="5.9,22.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8092,22.4155;5.9,22.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[5].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.9,22.65"/>
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
        <dia:string>#BKO-TBL[5].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O343">
      <dia:attribute name="obj_pos">
        <dia:point val="7.4,22.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3092,22.4155;7.4,22.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[5].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.4,22.65"/>
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
        <dia:string>#BKO-TBL[5].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O344">
      <dia:attribute name="obj_pos">
        <dia:point val="9.25,22.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7956,22.4155;9.25,22.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[5].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.25,22.65"/>
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
        <dia:string>#BKO-TBL[5].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O345">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,22.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.6138,22.4155;11.25,22.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[5].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,22.65"/>
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
        <dia:string>#BKO-TBL[5].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O346">
      <dia:attribute name="obj_pos">
        <dia:point val="5.9,23.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8092,22.9155;5.9,23.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[6].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.9,23.15"/>
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
        <dia:string>#BKO-TBL[6].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O347">
      <dia:attribute name="obj_pos">
        <dia:point val="7.4,23.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3092,22.9155;7.4,23.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[6].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.4,23.15"/>
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
        <dia:string>#BKO-TBL[6].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O348">
      <dia:attribute name="obj_pos">
        <dia:point val="9.25,23.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7956,22.9155;9.25,23.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[6].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.25,23.15"/>
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
        <dia:string>#BKO-TBL[6].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O349">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,23.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.6138,22.9155;11.25,23.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[6].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,23.15"/>
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
        <dia:string>#BKO-TBL[6].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O350">
      <dia:attribute name="obj_pos">
        <dia:point val="5.9,23.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8092,23.4155;5.9,23.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[7].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.9,23.65"/>
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
        <dia:string>#BKO-TBL[7].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O351">
      <dia:attribute name="obj_pos">
        <dia:point val="7.4,23.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3092,23.4155;7.4,23.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[7].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.4,23.65"/>
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
        <dia:string>#BKO-TBL[7].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O352">
      <dia:attribute name="obj_pos">
        <dia:point val="9.25,23.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7956,23.4155;9.25,23.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[7].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.25,23.65"/>
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
        <dia:string>#BKO-TBL[7].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O353">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,23.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.6138,23.4155;11.25,23.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[7].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,23.65"/>
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
        <dia:string>#BKO-TBL[7].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O354">
      <dia:attribute name="obj_pos">
        <dia:point val="5.9,24.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8092,23.9155;5.9,24.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[8].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.9,24.15"/>
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
        <dia:string>#BKO-TBL[8].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O355">
      <dia:attribute name="obj_pos">
        <dia:point val="7.4,24.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3092,23.9155;7.4,24.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[8].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.4,24.15"/>
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
        <dia:string>#BKO-TBL[8].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O356">
      <dia:attribute name="obj_pos">
        <dia:point val="9.25,24.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7956,23.9155;9.25,24.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[8].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.25,24.15"/>
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
        <dia:string>#BKO-TBL[8].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O357">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,24.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.6138,23.9155;11.25,24.2155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[8].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,24.15"/>
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
        <dia:string>#BKO-TBL[8].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O358">
      <dia:attribute name="obj_pos">
        <dia:point val="5.9,24.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8092,24.4155;5.9,24.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[9].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.9,24.65"/>
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
        <dia:string>#BKO-TBL[9].BKO-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O359">
      <dia:attribute name="obj_pos">
        <dia:point val="7.4,24.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3092,24.4155;7.4,24.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[9].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.4,24.65"/>
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
        <dia:string>#BKO-TBL[9].BKO-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="6"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O360">
      <dia:attribute name="obj_pos">
        <dia:point val="9.25,24.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7956,24.4155;9.25,24.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[9].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.25,24.65"/>
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
        <dia:string>#BKO-TBL[9].BKO-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O361">
      <dia:attribute name="obj_pos">
        <dia:point val="11.25,24.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.6138,24.4155;11.25,24.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#BKO-TBL[9].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.25,24.65"/>
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
        <dia:string>#BKO-TBL[9].BKO-KOJYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O362">
      <dia:attribute name="obj_pos">
        <dia:point val="18.4,1.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.4,1.28109;19.814,1.56109"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#TITLE1#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（返戻分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="18.4,1.5"/>
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
      <dia:attribute name="embed_id">
        <dia:string>#TITLE1#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O363">
      <dia:attribute name="obj_pos">
        <dia:point val="7.15,26.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.1502,26.6155;7.15,26.9155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#AVG-TBL[0].AVG#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.15,26.85"/>
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
        <dia:string>#AVG-TBL[0].AVG#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="11"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O364">
      <dia:attribute name="obj_pos">
        <dia:point val="7.15,27.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.1502,27.0155;7.15,27.3155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#AVG-TBL[1].AVG#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.15,27.25"/>
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
        <dia:string>#AVG-TBL[1].AVG#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="11"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O365">
      <dia:attribute name="obj_pos">
        <dia:point val="7.15,27.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.1502,27.4155;7.15,27.7155"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#AVG-TBL[2].AVG#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99999999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.15,27.65"/>
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
        <dia:string>#AVG-TBL[2].AVG#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="11"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
  </dia:layer>
</dia:diagram>
