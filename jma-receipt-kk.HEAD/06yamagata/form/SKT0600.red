<?xml version="1.0" encoding="UTF-8"?>
<dia:diagram xmlns:dia="http://www.lysator.liu.se/~alla/dia/">
  <dia:dictionarydata>
    <dia:element name="SEIYM" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="16"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="HOSPCD" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="7"/>
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
    <dia:element name="KAISETUNAME" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="40"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="TOKUBETU" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="10"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="DATA" occurs="1">
      <dia:element name="DATA-TBL" occurs="27">
        <dia:element name="HKNJANAME" occurs="1">
          <dia:appinfo>
            <dia:embed object="string" length="20"/>
          </dia:appinfo>
        </dia:element>
        <dia:element name="KAMIBUN" occurs="1">
          <dia:appinfo>
            <dia:embed object="string" length="12"/>
          </dia:appinfo>
        </dia:element>
        <dia:element name="NYUIN" occurs="1">
          <dia:element name="N-KENSU" occurs="1">
            <dia:appinfo>
              <dia:embed object="string" length="7"/>
            </dia:appinfo>
          </dia:element>
          <dia:element name="N-NISSU" occurs="1">
            <dia:appinfo>
              <dia:embed object="string" length="7"/>
            </dia:appinfo>
          </dia:element>
          <dia:element name="N-TENSU" occurs="1">
            <dia:appinfo>
              <dia:embed object="string" length="14"/>
            </dia:appinfo>
          </dia:element>
        </dia:element>
        <dia:element name="GAIRAI" occurs="1">
          <dia:element name="G-KENSU" occurs="1">
            <dia:appinfo>
              <dia:embed object="string" length="7"/>
            </dia:appinfo>
          </dia:element>
          <dia:element name="G-NISSU" occurs="1">
            <dia:appinfo>
              <dia:embed object="string" length="7"/>
            </dia:appinfo>
          </dia:element>
          <dia:element name="G-TENSU" occurs="1">
            <dia:appinfo>
              <dia:embed object="string" length="14"/>
            </dia:appinfo>
          </dia:element>
        </dia:element>
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
        <dia:real val="2.56579"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="0.742049"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.05"/>
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
    <dia:object type="Standard - Text" version="0" id="O63">
      <dia:attribute name="obj_pos">
        <dia:point val="7.61287,2.58944"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.61287,2.1913;18.2179,2.6913"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#診療（調剤）報酬・訪問看護療養費請求総括票#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O64">
      <dia:attribute name="obj_pos">
        <dia:point val="2.06012,3.76691"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.06012,3.48821;4.18112,3.83821"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#機関等コード#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
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
        <dia:point val="1.64691,4.6268"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.64691,4.38791;7.70691,4.68791"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#保険医療機関・薬局・訪問看護ステーション#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O66">
      <dia:attribute name="obj_pos">
        <dia:point val="5.06688,8.35666"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.06688,8.07796;6.12738,8.42796"/>
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
            <dia:real val="0.437500"/>
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
        <dia:point val="6.4295,8.32611"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.4295,8.08722;7.7021,8.38722"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#日(回)数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O68">
      <dia:attribute name="obj_pos">
        <dia:point val="8.36274,8.34771"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.36274,8.06901;10.0595,8.41901"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点   　数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
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
        <dia:point val="11.709,26.9285"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.709,26.6498;12.0625,26.9998"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#計#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
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
        <dia:point val="1.64691,5.4209"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.64691,5.18201;3.16191,5.48201"/>
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
            <dia:real val="0.375000"/>
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
        <dia:point val="1.58571,28.0646"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.58571,27.8257;9.76671,28.1257"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※県内分と県外分はそれぞれ別の用紙に記載してください。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
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
        <dia:point val="4.55,8.98625"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,8.78718;4.8025,9.28718"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O73">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,9.6872"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,9.48813;4.8025,10.2381"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O74">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,10.5759"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,10.3768;4.8025,10.8768"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O75">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,11.2768"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,11.0777;4.8025,11.8277"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O76">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,12.1525"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,11.9534;4.8025,12.4534"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O77">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,12.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,12.7009;4.8025,13.4509"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O78">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,13.7723"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,13.5732;4.8025,14.0732"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O79">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,14.4732"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,14.2741;4.8025,15.0241"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O80">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,15.3619"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,15.1628;4.8025,15.6628"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O81">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,16.0629"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,15.8638;4.8025,16.6138"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O82">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,16.9385"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,16.7394;4.8025,17.2394"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O83">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,17.6395"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,17.4404;4.8025,18.1904"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O84">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,18.4913"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,18.2922;4.8025,18.7922"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O85">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,19.1923"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,18.9932;4.8025,19.7432"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O86">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,20.0809"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,19.8818;4.8025,20.3818"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O87">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,20.7819"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,20.5828;4.8025,21.3328"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O88">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,21.6576"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,21.4585;4.8025,21.9585"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O89">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,22.3585"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,22.1594;4.8025,22.9094"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O90">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,23.2125"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,23.0134;4.8025,23.5134"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O91">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,23.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,23.7509;4.8025,24.5009"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O92">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,24.8021"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,24.603;4.8025,25.103"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O93">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,25.5031"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,25.304;4.8025,26.054"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O94">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,26.3787"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,26.1796;4.8025,26.6796"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O95">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55,27.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55,26.9009;4.8025,27.6509"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O96">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,4.28538"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,4.08631;13.7025,4.58631"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O97">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,4.98632"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,4.78725;13.7025,5.53725"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O98">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,5.87501"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,5.67594;13.7025,6.17594"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O99">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,6.57595"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,6.37688;13.7025,7.12688"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O100">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,7.45161"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,7.25254;13.7025,7.75254"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O101">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,8.15256"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,7.95349;13.7025,8.70349"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O102">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,9.07141"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,8.87234;13.7025,9.37234"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O103">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,9.71466"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,9.51559;13.7025,10.2656"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O104">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,10.661"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,10.4619;13.7025,10.9619"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O105">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,11.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,11.1009;13.7025,11.8509"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O106">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,12.2377"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,12.0386;13.7025,12.5386"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O107">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,12.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,12.7009;13.7025,13.4509"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O108">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,13.7904"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,13.5913;13.7025,14.0913"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O109">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,14.4914"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,14.2923;13.7025,15.0423"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O110">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,15.3801"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,15.181;13.7025,15.681"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O111">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,16.081"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,15.8819;13.7025,16.6319"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O112">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,16.9567"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,16.7576;13.7025,17.2576"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O113">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,17.6576"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,17.4585;13.7025,18.2085"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O114">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,18.5116"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,18.3125;13.7025,18.8125"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O115">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,19.2126"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,19.0135;13.7025,19.7635"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O116">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,20.1013"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,19.9022;13.7025,20.4022"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O117">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,20.8022"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,20.6031;13.7025,21.3531"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O118">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,21.6779"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,21.4788;13.7025,21.9788"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O119">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,22.3788"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,22.1797;13.7025,22.9297"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O120">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,23.2219"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,23.0228;13.7025,23.5228"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O121">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,23.9553"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,23.7562;13.7025,24.5062"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O122">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,24.8656"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,24.6665;13.7025,25.1665"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O123">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,25.5449"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,25.3458;13.7025,26.0958"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O124">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,26.4314"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,26.2323;13.7025,26.7323"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O125">
      <dia:attribute name="obj_pos">
        <dia:point val="13.45,27.0862"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.45,26.8871;13.7025,27.6371"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入
院
外#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O126">
      <dia:attribute name="obj_pos">
        <dia:point val="4.55594,8.19866"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.55594,7.99959;4.80844,8.49959"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#区
分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O127">
      <dia:attribute name="obj_pos">
        <dia:point val="2.38038,8.33512"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.38038,8.05642;3.79438,8.40642"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#保険者名#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.38038,8.33512"/>
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
    <dia:object type="Standard - Text" version="0" id="O128">
      <dia:attribute name="obj_pos">
        <dia:point val="14.0465,3.64147"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0465,3.36277;15.107,3.71277"/>
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
            <dia:real val="0.437500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.2121"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.17675"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O129">
      <dia:attribute name="obj_pos">
        <dia:point val="15.4091,3.61093"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.4091,3.37204;16.6817,3.67204"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#日(回)数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O130">
      <dia:attribute name="obj_pos">
        <dia:point val="17.2344,3.61093"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.2344,3.33223;18.9312,3.68223"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点   　数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.2121"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.17675"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O131">
      <dia:attribute name="obj_pos">
        <dia:point val="13.4484,3.44314"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.4484,3.24407;13.7009,3.74407"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#区
分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O132">
      <dia:attribute name="obj_pos">
        <dia:point val="11.3043,3.5921"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.3043,3.3134;12.7183,3.6634"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#保険者名#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.3043,3.5921"/>
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
    <dia:object type="Standard - Text" version="0" id="O133">
      <dia:attribute name="obj_pos">
        <dia:point val="17.2944,1.64771"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.2944,1.20975;18.7387,1.75975"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#国 保#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.687500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.3333"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.27775"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O134">
      <dia:attribute name="obj_pos">
        <dia:point val="9.89371,6.97388"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.89371,6.69518;10.2472,7.04518"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#印#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.2121"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.17675"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O135">
      <dia:attribute name="obj_pos">
        <dia:point val="4.85159,2.62252"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.85159,2.22438;5.35659,2.72438"/>
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
            <dia:real val="0.625000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O136">
      <dia:attribute name="obj_pos">
        <dia:point val="1.70671,1.7038"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.70671,1.34547;5.34271,1.79547"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#【国民健康保険】#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.2727"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.22725"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O137">
      <dia:attribute name="obj_pos">
        <dia:point val="1.64691,5.04278"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.64691,4.80389;4.07091,5.10389"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#の所在地及び名称#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
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
    <dia:object type="Embed - Text" version="0" id="O138">
      <dia:attribute name="obj_pos">
        <dia:point val="1.86651,11.1156"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86651,10.9245;4.29051,11.1645"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[1].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[1].HKNJANAME#</dia:string>
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
        <dia:point val="1.86651,12.778"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86651,12.5869;4.29051,12.8269"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[2].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[2].HKNJANAME#</dia:string>
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
        <dia:point val="1.86651,14.4208"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86651,14.2297;4.29051,14.4697"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[3].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[3].HKNJANAME#</dia:string>
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
        <dia:point val="1.86651,15.9076"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86651,15.7165;4.29051,15.9565"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[4].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[4].HKNJANAME#</dia:string>
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
        <dia:point val="1.84883,17.5296"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.84883,17.3385;4.27283,17.5785"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[5].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[5].HKNJANAME#</dia:string>
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
        <dia:point val="1.86651,19.1114"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86651,18.9203;4.29051,19.1603"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[6].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[6].HKNJANAME#</dia:string>
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
        <dia:point val="1.89751,20.657"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.89751,20.4659;4.32151,20.7059"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[7].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[7].HKNJANAME#</dia:string>
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
        <dia:point val="1.92852,22.2331"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.92852,22.042;4.35252,22.282"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[8].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[8].HKNJANAME#</dia:string>
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
        <dia:point val="1.86651,23.8027"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86651,23.6116;4.29051,23.8516"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[9].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[9].HKNJANAME#</dia:string>
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
        <dia:point val="1.86651,25.3554"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86651,25.1643;4.29051,25.4043"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[10].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[10].HKNJANAME#</dia:string>
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
        <dia:point val="1.86651,26.9357"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86651,26.7446;4.29051,26.9846"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[11].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[11].HKNJANAME#</dia:string>
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
        <dia:point val="1.86651,9.67763"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.86651,9.48652;4.29051,9.72652"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[0].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[0].HKNJANAME#</dia:string>
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
        <dia:point val="4.80459,2.6137"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.57259,2.21555;4.80459,2.71555"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SEIYM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#平成99年99月#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.303"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.2525"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#SEIYM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="16"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O151">
      <dia:attribute name="obj_pos">
        <dia:point val="4.06282,5.3472"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.06282,5.14813;10.3753,5.64813"/>
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
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
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
    <dia:object type="Embed - Text" version="0" id="O152">
      <dia:attribute name="obj_pos">
        <dia:point val="4.04516,6.10073"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.04516,5.90166;9.09516,6.40166"/>
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
            <dia:real val="0.312500"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1515"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.12625"/>
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
    <dia:object type="Embed - Text" version="0" id="O153">
      <dia:attribute name="obj_pos">
        <dia:point val="4.0409,6.92933"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.0409,6.70637;9.6969,6.98637"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KAISETUNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああああああああああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.350000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.16968"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1414"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#KAISETUNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="40"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O154">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7473,6.41976"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,6.22865;13.1713,6.46865"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[13].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[13].HKNJANAME#</dia:string>
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
        <dia:point val="10.7473,8.10783"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,7.91672;13.1713,8.15672"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[14].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[14].HKNJANAME#</dia:string>
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
        <dia:point val="10.7473,9.67763"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,9.48652;13.1713,9.72652"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[15].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[15].HKNJANAME#</dia:string>
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
        <dia:point val="10.7473,11.1156"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,10.9245;13.1713,11.1645"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[16].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[16].HKNJANAME#</dia:string>
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
        <dia:point val="10.7473,12.778"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,12.5869;13.1713,12.8269"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[17].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[17].HKNJANAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O159">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7473,14.4208"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,14.2297;13.1713,14.4697"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[18].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[18].HKNJANAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O160">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7783,15.9076"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7783,15.7165;13.2023,15.9565"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[19].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[19].HKNJANAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O161">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7916,17.5296"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7916,17.3385;13.2156,17.5785"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[20].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[20].HKNJANAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O162">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7473,19.1114"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,18.9203;13.1713,19.1603"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[21].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[21].HKNJANAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O163">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7473,20.657"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,20.4659;13.1713,20.7059"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[22].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[22].HKNJANAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O164">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7473,22.2508"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,22.0597;13.1713,22.2997"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[23].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[23].HKNJANAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O165">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7473,5.02295"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7473,4.83184;13.1713,5.07184"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[12].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[12].HKNJANAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O166">
      <dia:attribute name="obj_pos">
        <dia:point val="10.8093,23.8204"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.8093,23.6293;13.2333,23.8693"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[24].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[24].HKNJANAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O167">
      <dia:attribute name="obj_pos">
        <dia:point val="10.8093,25.3554"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.8093,25.1643;13.2333,25.4043"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[25].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[25].HKNJANAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O168">
      <dia:attribute name="obj_pos">
        <dia:point val="5.95,3.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.95,3.32222;8.4952,3.92222"/>
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
            <dia:string>#9999999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.750000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.3636"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.303"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#HOSPCD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
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
    <dia:object type="Embed - Text" version="0" id="O170">
      <dia:attribute name="obj_pos">
        <dia:point val="10.8,26.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.8,26.3089;13.224,26.5489"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[26].HKNJANAME#</dia:string>
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
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[26].HKNJANAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="20"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O171">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,9.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,8.96111;10.3,9.26111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[0].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,9.2"/>
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
        <dia:string>#DATA.DATA-TBL[0].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O172">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,9.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,8.96111;6.1,9.26111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[0].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,9.2"/>
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
        <dia:string>#DATA.DATA-TBL[0].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O173">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,9.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,8.96111;7.6,9.26111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[0].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,9.2"/>
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
        <dia:string>#DATA.DATA-TBL[0].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O174">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,9.71111;6.1,10.0111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[0].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,9.95"/>
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
        <dia:string>#DATA.DATA-TBL[0].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O175">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,9.71111;7.6,10.0111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[0].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,9.95"/>
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
        <dia:string>#DATA.DATA-TBL[0].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O176">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,9.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,9.71111;10.3,10.0111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[0].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,9.95"/>
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
        <dia:string>#DATA.DATA-TBL[0].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O177">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,10.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,10.5111;10.3,10.8111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[1].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,10.75"/>
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
        <dia:string>#DATA.DATA-TBL[1].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O178">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,10.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,10.5111;6.1,10.8111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[1].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,10.75"/>
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
        <dia:string>#DATA.DATA-TBL[1].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O179">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,10.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,10.5111;7.6,10.8111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[1].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,10.75"/>
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
        <dia:string>#DATA.DATA-TBL[1].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O180">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,11.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,11.3111;6.1,11.6111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[1].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,11.55"/>
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
        <dia:string>#DATA.DATA-TBL[1].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O181">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,11.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,11.3111;7.6,11.6111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[1].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,11.55"/>
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
        <dia:string>#DATA.DATA-TBL[1].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O182">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,11.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,11.3111;10.3,11.6111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[1].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,11.55"/>
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
        <dia:string>#DATA.DATA-TBL[1].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O183">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,12.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,12.1111;10.3,12.4111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[2].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,12.35"/>
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
        <dia:string>#DATA.DATA-TBL[2].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O184">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,12.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,12.1111;6.1,12.4111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[2].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,12.35"/>
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
        <dia:string>#DATA.DATA-TBL[2].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O185">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,12.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,12.1111;7.6,12.4111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[2].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,12.35"/>
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
        <dia:string>#DATA.DATA-TBL[2].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O186">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,13.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,12.9111;6.1,13.2111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[2].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,13.15"/>
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
        <dia:string>#DATA.DATA-TBL[2].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O187">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,13.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,12.9111;7.6,13.2111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[2].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,13.15"/>
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
        <dia:string>#DATA.DATA-TBL[2].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O188">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,13.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,12.9111;10.3,13.2111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[2].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,13.15"/>
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
        <dia:string>#DATA.DATA-TBL[2].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O189">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,13.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,13.6611;10.3,13.9611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[3].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,13.9"/>
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
        <dia:string>#DATA.DATA-TBL[3].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O190">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,13.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,13.6611;6.1,13.9611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[3].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,13.9"/>
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
        <dia:string>#DATA.DATA-TBL[3].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O191">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,13.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,13.6611;7.6,13.9611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[3].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,13.9"/>
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
        <dia:string>#DATA.DATA-TBL[3].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O192">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,14.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,14.4611;6.1,14.7611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[3].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,14.7"/>
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
        <dia:string>#DATA.DATA-TBL[3].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O193">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,14.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,14.4611;7.6,14.7611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[3].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,14.7"/>
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
        <dia:string>#DATA.DATA-TBL[3].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O194">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,14.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,14.4611;10.3,14.7611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[3].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,14.7"/>
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
        <dia:string>#DATA.DATA-TBL[3].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O195">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,15.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,15.2611;10.3,15.5611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[4].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,15.5"/>
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
        <dia:string>#DATA.DATA-TBL[4].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O196">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,15.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,15.2611;6.1,15.5611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[4].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,15.5"/>
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
        <dia:string>#DATA.DATA-TBL[4].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O197">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,15.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,15.2611;7.6,15.5611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[4].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,15.5"/>
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
        <dia:string>#DATA.DATA-TBL[4].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O198">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,16.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,16.0611;6.1,16.3611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[4].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,16.3"/>
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
        <dia:string>#DATA.DATA-TBL[4].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O199">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,16.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,16.0611;7.6,16.3611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[4].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,16.3"/>
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
        <dia:string>#DATA.DATA-TBL[4].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O200">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,16.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,16.0611;10.3,16.3611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[4].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,16.3"/>
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
        <dia:string>#DATA.DATA-TBL[4].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O201">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,17.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,16.8611;10.3,17.1611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[5].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,17.1"/>
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
        <dia:string>#DATA.DATA-TBL[5].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O202">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,17.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,16.8611;6.1,17.1611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[5].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,17.1"/>
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
        <dia:string>#DATA.DATA-TBL[5].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O203">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,17.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,16.8611;7.6,17.1611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[5].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,17.1"/>
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
        <dia:string>#DATA.DATA-TBL[5].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O204">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,17.6611;6.1,17.9611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[5].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.1818"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1515"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[5].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O205">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,17.6611;7.6,17.9611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[5].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,17.9"/>
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
        <dia:string>#DATA.DATA-TBL[5].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O206">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,17.6611;10.3,17.9611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[5].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,17.9"/>
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
        <dia:string>#DATA.DATA-TBL[5].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O207">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,18.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,18.4111;10.3,18.7111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[6].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,18.65"/>
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
        <dia:string>#DATA.DATA-TBL[6].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O208">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,18.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,18.4111;6.1,18.7111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[6].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,18.65"/>
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
        <dia:string>#DATA.DATA-TBL[6].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O209">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,18.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,18.4111;7.6,18.7111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[6].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,18.65"/>
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
        <dia:string>#DATA.DATA-TBL[6].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O210">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,19.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,19.2111;6.1,19.5111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[6].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,19.45"/>
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
        <dia:string>#DATA.DATA-TBL[6].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O211">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,19.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,19.2111;7.6,19.5111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[6].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,19.45"/>
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
        <dia:string>#DATA.DATA-TBL[6].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O212">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,19.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,19.2111;10.3,19.5111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[6].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,19.45"/>
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
        <dia:string>#DATA.DATA-TBL[6].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O213">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,20.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,20.0111;10.3,20.3111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[7].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,20.25"/>
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
        <dia:string>#DATA.DATA-TBL[7].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O214">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,20.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,20.0111;6.1,20.3111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[7].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,20.25"/>
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
        <dia:string>#DATA.DATA-TBL[7].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O215">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,20.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,20.0111;7.6,20.3111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[7].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,20.25"/>
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
        <dia:string>#DATA.DATA-TBL[7].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O216">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,21.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,20.8111;6.1,21.1111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[7].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,21.05"/>
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
        <dia:string>#DATA.DATA-TBL[7].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O217">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,21.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,20.8111;7.6,21.1111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[7].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,21.05"/>
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
        <dia:string>#DATA.DATA-TBL[7].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O218">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,21.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,20.8111;10.3,21.1111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[7].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,21.05"/>
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
        <dia:string>#DATA.DATA-TBL[7].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O219">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,21.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,21.6111;10.3,21.9111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[8].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,21.85"/>
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
        <dia:string>#DATA.DATA-TBL[8].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O220">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,21.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,21.6111;6.1,21.9111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[8].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,21.85"/>
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
        <dia:string>#DATA.DATA-TBL[8].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O221">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,21.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,21.6111;7.6,21.9111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[8].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,21.85"/>
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
        <dia:string>#DATA.DATA-TBL[8].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O222">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,22.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,22.4111;6.1,22.7111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[8].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,22.65"/>
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
        <dia:string>#DATA.DATA-TBL[8].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O223">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,22.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,22.4111;7.6,22.7111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[8].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,22.65"/>
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
        <dia:string>#DATA.DATA-TBL[8].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O224">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,22.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,22.4111;10.3,22.7111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[8].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,22.65"/>
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
        <dia:string>#DATA.DATA-TBL[8].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O225">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,23.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,23.1611;10.3,23.4611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[9].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,23.4"/>
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
        <dia:string>#DATA.DATA-TBL[9].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O226">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,23.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,23.1611;6.1,23.4611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[9].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,23.4"/>
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
        <dia:string>#DATA.DATA-TBL[9].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O227">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,23.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,23.1611;7.6,23.4611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[9].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,23.4"/>
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
        <dia:string>#DATA.DATA-TBL[9].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O228">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,24.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,23.9611;6.1,24.2611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[9].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,24.2"/>
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
        <dia:string>#DATA.DATA-TBL[9].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O229">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,24.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,23.9611;7.6,24.2611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[9].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,24.2"/>
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
        <dia:string>#DATA.DATA-TBL[9].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O230">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,24.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,23.9611;10.3,24.2611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[9].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,24.2"/>
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
        <dia:string>#DATA.DATA-TBL[9].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O231">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,24.7611;10.3,25.0611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[10].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,25"/>
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
        <dia:string>#DATA.DATA-TBL[10].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O232">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,24.7611;6.1,25.0611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[10].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,25"/>
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
        <dia:string>#DATA.DATA-TBL[10].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O233">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,24.7611;7.6,25.0611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[10].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,25"/>
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
        <dia:string>#DATA.DATA-TBL[10].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O234">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,25.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,25.5611;6.1,25.8611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[10].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,25.8"/>
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
        <dia:string>#DATA.DATA-TBL[10].GAIRAI.G-KENSU#</dia:string>
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
        <dia:point val="7.6,25.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,25.5611;7.6,25.8611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[10].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,25.8"/>
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
        <dia:string>#DATA.DATA-TBL[10].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O236">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,25.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,25.5611;10.3,25.8611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[10].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,25.8"/>
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
        <dia:string>#DATA.DATA-TBL[10].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O237">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,26.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,26.3111;10.3,26.6111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[11].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,26.55"/>
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
        <dia:string>#DATA.DATA-TBL[11].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O238">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,26.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,26.3111;6.1,26.6111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[11].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,26.55"/>
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
        <dia:string>#DATA.DATA-TBL[11].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O239">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,26.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,26.3111;7.6,26.6111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[11].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,26.55"/>
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
        <dia:string>#DATA.DATA-TBL[11].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O240">
      <dia:attribute name="obj_pos">
        <dia:point val="6.1,27.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.8274,27.1111;6.1,27.4111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[11].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.1,27.35"/>
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
        <dia:string>#DATA.DATA-TBL[11].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O241">
      <dia:attribute name="obj_pos">
        <dia:point val="7.6,27.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.3274,27.1111;7.6,27.4111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[11].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.6,27.35"/>
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
        <dia:string>#DATA.DATA-TBL[11].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O242">
      <dia:attribute name="obj_pos">
        <dia:point val="10.3,27.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.7548,27.1111;10.3,27.4111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[11].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.3,27.35"/>
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
        <dia:string>#DATA.DATA-TBL[11].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O243">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,4.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,4.21111;19.175,4.51111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[12].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,4.45"/>
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
        <dia:string>#DATA.DATA-TBL[12].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O244">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,4.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,4.21111;15.1,4.51111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[12].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,4.45"/>
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
        <dia:string>#DATA.DATA-TBL[12].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O245">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,4.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,4.21111;16.55,4.51111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[12].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,4.45"/>
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
        <dia:string>#DATA.DATA-TBL[12].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O246">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,5.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,5.01111;15.1,5.31111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[12].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,5.25"/>
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
        <dia:string>#DATA.DATA-TBL[12].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O247">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,5.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,5.01111;16.55,5.31111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[12].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,5.25"/>
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
        <dia:string>#DATA.DATA-TBL[12].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O248">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,5.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,5.01111;19.175,5.31111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[12].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,5.25"/>
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
        <dia:string>#DATA.DATA-TBL[12].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O249">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,6.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,5.81111;19.175,6.11111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[13].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,6.05"/>
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
        <dia:string>#DATA.DATA-TBL[13].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O250">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,6.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,5.81111;15.1,6.11111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[13].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,6.05"/>
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
        <dia:string>#DATA.DATA-TBL[13].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O251">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,6.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,5.81111;16.55,6.11111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[13].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,6.05"/>
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
        <dia:string>#DATA.DATA-TBL[13].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O252">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,6.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,6.61111;15.1,6.91111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[13].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,6.85"/>
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
        <dia:string>#DATA.DATA-TBL[13].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O253">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,6.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,6.61111;16.55,6.91111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[13].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,6.85"/>
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
        <dia:string>#DATA.DATA-TBL[13].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O254">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,6.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,6.61111;19.175,6.91111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[13].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,6.85"/>
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
        <dia:string>#DATA.DATA-TBL[13].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O255">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,7.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,7.36111;19.175,7.66111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[14].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,7.6"/>
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
        <dia:string>#DATA.DATA-TBL[14].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O256">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,7.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,7.36111;15.1,7.66111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[14].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,7.6"/>
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
        <dia:string>#DATA.DATA-TBL[14].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O257">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,7.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,7.36111;16.55,7.66111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[14].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,7.6"/>
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
        <dia:string>#DATA.DATA-TBL[14].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O258">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,8.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,8.16111;15.1,8.46111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[14].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,8.4"/>
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
        <dia:string>#DATA.DATA-TBL[14].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O259">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,8.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,8.16111;16.55,8.46111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[14].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,8.4"/>
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
        <dia:string>#DATA.DATA-TBL[14].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O260">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,8.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,8.16111;19.175,8.46111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[14].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,8.4"/>
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
        <dia:string>#DATA.DATA-TBL[14].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O261">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,9.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,9.01111;19.175,9.31111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[15].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,9.25"/>
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
        <dia:string>#DATA.DATA-TBL[15].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O262">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,9.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,9.01111;15.1,9.31111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[15].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,9.25"/>
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
        <dia:string>#DATA.DATA-TBL[15].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O263">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,9.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,9.01111;16.55,9.31111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[15].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,9.25"/>
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
        <dia:string>#DATA.DATA-TBL[15].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O264">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,10.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,9.81111;15.1,10.1111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[15].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,10.05"/>
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
        <dia:string>#DATA.DATA-TBL[15].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O265">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,10.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,9.81111;16.55,10.1111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[15].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,10.05"/>
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
        <dia:string>#DATA.DATA-TBL[15].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O266">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,10.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,9.81111;19.175,10.1111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[15].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,10.05"/>
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
        <dia:string>#DATA.DATA-TBL[15].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O267">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,10.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,10.5111;19.175,10.8111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[16].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,10.75"/>
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
        <dia:string>#DATA.DATA-TBL[16].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O268">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,10.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,10.5111;15.1,10.8111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[16].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,10.75"/>
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
        <dia:string>#DATA.DATA-TBL[16].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O269">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,10.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,10.5111;16.55,10.8111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[16].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,10.75"/>
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
        <dia:string>#DATA.DATA-TBL[16].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O270">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,11.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,11.3111;15.1,11.6111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[16].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,11.55"/>
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
        <dia:string>#DATA.DATA-TBL[16].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O271">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,11.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,11.3111;16.55,11.6111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[16].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,11.55"/>
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
        <dia:string>#DATA.DATA-TBL[16].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O272">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,11.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,11.3111;19.175,11.6111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[16].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,11.55"/>
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
        <dia:string>#DATA.DATA-TBL[16].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O273">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,12.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,12.1111;19.175,12.4111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[17].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,12.35"/>
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
        <dia:string>#DATA.DATA-TBL[17].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O274">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,12.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,12.1111;15.1,12.4111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[17].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,12.35"/>
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
        <dia:string>#DATA.DATA-TBL[17].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O275">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,12.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,12.1111;16.55,12.4111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[17].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,12.35"/>
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
        <dia:string>#DATA.DATA-TBL[17].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O276">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,13.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,12.9111;15.1,13.2111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[17].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,13.15"/>
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
        <dia:string>#DATA.DATA-TBL[17].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O277">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,13.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,12.9111;16.55,13.2111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[17].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,13.15"/>
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
        <dia:string>#DATA.DATA-TBL[17].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O278">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,13.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,12.9111;19.175,13.2111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[17].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,13.15"/>
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
        <dia:string>#DATA.DATA-TBL[17].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O279">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,13.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,13.6611;19.175,13.9611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[18].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,13.9"/>
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
        <dia:string>#DATA.DATA-TBL[18].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O280">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,13.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,13.6611;15.1,13.9611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[18].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,13.9"/>
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
        <dia:string>#DATA.DATA-TBL[18].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O281">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,13.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,13.6611;16.55,13.9611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[18].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,13.9"/>
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
        <dia:string>#DATA.DATA-TBL[18].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O282">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,14.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,14.4611;15.1,14.7611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[18].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,14.7"/>
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
        <dia:string>#DATA.DATA-TBL[18].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O283">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,14.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,14.4611;16.55,14.7611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[18].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,14.7"/>
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
        <dia:string>#DATA.DATA-TBL[18].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O284">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,14.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,14.4611;19.175,14.7611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[18].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,14.7"/>
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
        <dia:string>#DATA.DATA-TBL[18].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O285">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,15.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,15.2611;19.175,15.5611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[19].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,15.5"/>
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
        <dia:string>#DATA.DATA-TBL[19].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O286">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,15.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,15.2611;15.1,15.5611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[19].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,15.5"/>
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
        <dia:string>#DATA.DATA-TBL[19].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O287">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,15.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,15.2611;16.55,15.5611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[19].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,15.5"/>
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
        <dia:string>#DATA.DATA-TBL[19].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O288">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,16.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,16.0611;15.1,16.3611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[19].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,16.3"/>
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
        <dia:string>#DATA.DATA-TBL[19].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O289">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,16.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,16.0611;16.55,16.3611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[19].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,16.3"/>
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
        <dia:string>#DATA.DATA-TBL[19].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O290">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,16.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,16.0611;19.175,16.3611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[19].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,16.3"/>
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
        <dia:string>#DATA.DATA-TBL[19].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O291">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,17.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,16.8611;19.175,17.1611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[20].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,17.1"/>
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
        <dia:string>#DATA.DATA-TBL[20].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O292">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,17.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,16.8611;15.1,17.1611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[20].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,17.1"/>
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
        <dia:string>#DATA.DATA-TBL[20].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O293">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,17.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,16.8611;16.55,17.1611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[20].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,17.1"/>
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
        <dia:string>#DATA.DATA-TBL[20].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O294">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,17.6611;15.1,17.9611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[20].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,17.9"/>
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
        <dia:string>#DATA.DATA-TBL[20].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O295">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,17.6611;16.55,17.9611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[20].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,17.9"/>
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
        <dia:string>#DATA.DATA-TBL[20].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O296">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,17.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,17.6611;19.175,17.9611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[20].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,17.9"/>
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
        <dia:string>#DATA.DATA-TBL[20].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O297">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,18.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,18.4111;19.175,18.7111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[21].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,18.65"/>
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
        <dia:string>#DATA.DATA-TBL[21].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O298">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,18.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,18.4111;15.1,18.7111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[21].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,18.65"/>
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
        <dia:string>#DATA.DATA-TBL[21].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O299">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,18.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,18.4111;16.55,18.7111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[21].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,18.65"/>
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
        <dia:string>#DATA.DATA-TBL[21].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O300">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,19.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,19.2111;15.1,19.5111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[21].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,19.45"/>
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
        <dia:string>#DATA.DATA-TBL[21].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O301">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,19.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,19.2111;16.55,19.5111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[21].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,19.45"/>
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
        <dia:string>#DATA.DATA-TBL[21].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O302">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,19.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,19.2111;19.175,19.5111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[21].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,19.45"/>
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
        <dia:string>#DATA.DATA-TBL[21].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O303">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,20.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,20.0111;19.175,20.3111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[22].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,20.25"/>
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
        <dia:string>#DATA.DATA-TBL[22].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O304">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,20.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,20.0111;15.1,20.3111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[22].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,20.25"/>
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
        <dia:string>#DATA.DATA-TBL[22].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O305">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,20.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,20.0111;16.55,20.3111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[22].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,20.25"/>
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
        <dia:string>#DATA.DATA-TBL[22].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O306">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,21.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,20.8111;15.1,21.1111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[22].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,21.05"/>
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
        <dia:string>#DATA.DATA-TBL[22].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O307">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,21.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,20.8111;16.55,21.1111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[22].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,21.05"/>
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
        <dia:string>#DATA.DATA-TBL[22].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O308">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,21.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,20.8111;19.175,21.1111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[22].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,21.05"/>
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
        <dia:string>#DATA.DATA-TBL[22].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O309">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,21.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,21.5611;19.175,21.8611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[23].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,21.8"/>
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
        <dia:string>#DATA.DATA-TBL[23].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O310">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,21.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,21.5611;15.1,21.8611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[23].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,21.8"/>
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
        <dia:string>#DATA.DATA-TBL[23].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O311">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,21.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,21.5611;16.55,21.8611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[23].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,21.8"/>
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
        <dia:string>#DATA.DATA-TBL[23].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O312">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,22.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,22.3611;15.1,22.6611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[23].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,22.6"/>
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
        <dia:string>#DATA.DATA-TBL[23].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O313">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,22.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,22.3611;16.55,22.6611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[23].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,22.6"/>
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
        <dia:string>#DATA.DATA-TBL[23].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O314">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,22.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,22.3611;19.175,22.6611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[23].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,22.6"/>
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
        <dia:string>#DATA.DATA-TBL[23].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O315">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,23.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,23.2111;19.175,23.5111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[24].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,23.45"/>
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
        <dia:string>#DATA.DATA-TBL[24].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O316">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,23.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,23.2111;15.1,23.5111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[24].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,23.45"/>
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
        <dia:string>#DATA.DATA-TBL[24].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O317">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,23.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,23.2111;16.55,23.5111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[24].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,23.45"/>
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
        <dia:string>#DATA.DATA-TBL[24].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O318">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,24.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,24.0111;15.1,24.3111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[24].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,24.25"/>
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
        <dia:string>#DATA.DATA-TBL[24].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O319">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,24.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,24.0111;16.55,24.3111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[24].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,24.25"/>
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
        <dia:string>#DATA.DATA-TBL[24].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O320">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,24.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,24.0111;19.175,24.3111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[24].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,24.25"/>
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
        <dia:string>#DATA.DATA-TBL[24].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O321">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,24.7611;19.175,25.0611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[25].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,25"/>
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
        <dia:string>#DATA.DATA-TBL[25].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O322">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,24.7611;15.1,25.0611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[25].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,25"/>
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
        <dia:string>#DATA.DATA-TBL[25].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O323">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,24.7611;16.55,25.0611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[25].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,25"/>
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
        <dia:string>#DATA.DATA-TBL[25].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O324">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,25.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,25.5611;15.1,25.8611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[25].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,25.8"/>
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
        <dia:string>#DATA.DATA-TBL[25].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O325">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,25.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,25.5611;16.55,25.8611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[25].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,25.8"/>
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
        <dia:string>#DATA.DATA-TBL[25].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O326">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,25.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,25.5611;19.175,25.8611"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[25].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,25.8"/>
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
        <dia:string>#DATA.DATA-TBL[25].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O327">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,26.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,26.3111;19.175,26.6111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[26].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,26.55"/>
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
        <dia:string>#DATA.DATA-TBL[26].NYUIN.N-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O328">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,26.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,26.3111;15.1,26.6111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[26].NYUIN.N-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,26.55"/>
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
        <dia:string>#DATA.DATA-TBL[26].NYUIN.N-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O329">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,26.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,26.3111;16.55,26.6111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[26].NYUIN.N-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,26.55"/>
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
        <dia:string>#DATA.DATA-TBL[26].NYUIN.N-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O330">
      <dia:attribute name="obj_pos">
        <dia:point val="15.1,27.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.8274,27.1111;15.1,27.4111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[26].GAIRAI.G-KENSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.1,27.35"/>
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
        <dia:string>#DATA.DATA-TBL[26].GAIRAI.G-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O331">
      <dia:attribute name="obj_pos">
        <dia:point val="16.55,27.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.2774,27.1111;16.55,27.4111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[26].GAIRAI.G-NISSU#</dia:string>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.55,27.35"/>
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
        <dia:string>#DATA.DATA-TBL[26].GAIRAI.G-NISSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O332">
      <dia:attribute name="obj_pos">
        <dia:point val="19.175,27.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="16.6298,27.1111;19.175,27.4111"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[26].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99,999,999,999#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.175,27.35"/>
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
        <dia:string>#DATA.DATA-TBL[26].GAIRAI.G-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="14"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O333">
      <dia:attribute name="obj_pos">
        <dia:point val="1.87471,10.1152"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,9.92409;3.32911,10.1641"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[0].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[0].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O334">
      <dia:attribute name="obj_pos">
        <dia:point val="1.87471,11.5862"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,11.3951;3.32911,11.6351"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[1].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[1].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O335">
      <dia:attribute name="obj_pos">
        <dia:point val="1.87471,13.2362"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,13.0451;3.32911,13.2851"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[2].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[2].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O336">
      <dia:attribute name="obj_pos">
        <dia:point val="1.87471,14.7862"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,14.5951;3.32911,14.8351"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[3].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[3].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O337">
      <dia:attribute name="obj_pos">
        <dia:point val="1.87471,16.3862"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,16.1951;3.32911,16.4351"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[4].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[4].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O338">
      <dia:attribute name="obj_pos">
        <dia:point val="1.87471,17.9112"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,17.7201;3.32911,17.9601"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[5].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[5].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O339">
      <dia:attribute name="obj_pos">
        <dia:point val="1.87471,19.4862"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,19.2951;3.32911,19.5351"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[6].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[6].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O340">
      <dia:attribute name="obj_pos">
        <dia:point val="1.87471,21.0612"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,20.8701;3.32911,21.1101"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[7].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[7].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O341">
      <dia:attribute name="obj_pos">
        <dia:point val="1.87471,22.6711"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,22.48;3.32911,22.72"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[8].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[8].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O342">
      <dia:attribute name="obj_pos">
        <dia:point val="1.87471,24.2461"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,24.055;3.32911,24.295"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[9].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[9].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O343">
      <dia:attribute name="obj_pos">
        <dia:point val="1.87471,25.7211"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,25.53;3.32911,25.77"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[10].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[10].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O344">
      <dia:attribute name="obj_pos">
        <dia:point val="1.87471,27.3711"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.87471,27.18;3.32911,27.42"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[11].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[11].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O345">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7589,5.3708"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,5.17969;12.2133,5.41969"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[12].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[12].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O346">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7589,6.85475"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,6.66364;12.2133,6.90364"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[13].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[13].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O347">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7589,8.48454"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,8.29343;12.2133,8.53343"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[14].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[14].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O348">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7589,10.0531"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,9.86199;12.2133,10.102"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[15].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[15].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O349">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7589,11.5281"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,11.337;12.2133,11.577"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[16].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[16].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O350">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7589,13.2531"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,13.062;12.2133,13.302"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[17].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[17].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O351">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7589,14.8096"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,14.6185;12.2133,14.8585"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[18].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[18].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O352">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7589,16.3346"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,16.1435;12.2133,16.3835"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[19].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[19].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O353">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7589,17.9096"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,17.7185;12.2133,17.9585"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[20].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[20].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O354">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7589,19.5461"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,19.355;12.2133,19.595"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[21].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[21].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O355">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7589,21.0961"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,20.905;12.2133,21.145"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[22].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[22].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O356">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7589,22.6961"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,22.505;12.2133,22.745"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[23].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[23].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O357">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7589,24.2711"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,24.08;12.2133,24.32"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[24].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[24].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O358">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7589,25.7461"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,25.555;12.2133,25.795"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[25].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[25].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O359">
      <dia:attribute name="obj_pos">
        <dia:point val="10.7589,27.2711"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="10.7589,27.08;12.2133,27.32"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#DATA.DATA-TBL[26].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#（紙提出分）#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.300000"/>
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
          <dia:attribute name="single">
            <dia:real val="0.14544"/>
          </dia:attribute>
          <dia:attribute name="multi">
            <dia:real val="0.1212"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="embed_id">
        <dia:string>#DATA.DATA-TBL[26].KAMIBUN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="12"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O360">
      <dia:attribute name="obj_pos">
        <dia:point val="9.25801,1.30539"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.25801,0.986871;11.278,1.38687"/>
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
            <dia:point val="9.25801,1.30539"/>
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
