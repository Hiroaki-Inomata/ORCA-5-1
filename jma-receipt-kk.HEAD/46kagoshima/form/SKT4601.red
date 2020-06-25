<?xml version="1.0" encoding="UTF-8"?>
<dia:diagram xmlns:dia="http://www.lysator.liu.se/~alla/dia/">
  <dia:dictionarydata>
    <dia:element name="HOSPCD" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="7"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="TOKUBETU" occurs="1">
      <dia:appinfo>
        <dia:embed object="string" length="10"/>
      </dia:appinfo>
    </dia:element>
    <dia:element name="KOHI" occurs="10">
      <dia:element name="HBTNUM" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="2"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="KOHNAME" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="10"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="NYUGAI-TBL" occurs="2">
        <dia:element name="KENSU" occurs="1">
          <dia:appinfo>
            <dia:embed object="string" length="5"/>
          </dia:appinfo>
        </dia:element>
        <dia:element name="ITBFTN" occurs="1">
          <dia:appinfo>
            <dia:embed object="string" length="9"/>
          </dia:appinfo>
        </dia:element>
      </dia:element>
    </dia:element>
    <dia:element name="SHOKUJI" occurs="10">
      <dia:element name="SKENSU" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="5"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="SKINGK" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="9"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="SFTNGK" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="9"/>
        </dia:appinfo>
      </dia:element>
    </dia:element>
    <dia:element name="ZAITAKU" occurs="7">
      <dia:element name="Z-KENSU" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="5"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="Z-TENSU" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="10"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="Z-ITBFTN" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="9"/>
        </dia:appinfo>
      </dia:element>
    </dia:element>
    <dia:element name="ZAITAKU-KOHI" occurs="3">
      <dia:element name="Z-HBTNUM" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="2"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="Z-KOHNAME" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="10"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="ZK-KENSU" occurs="1">
        <dia:appinfo>
          <dia:embed object="string" length="5"/>
        </dia:appinfo>
      </dia:element>
      <dia:element name="ZK-ITBFTN" occurs="1">
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
          <dia:real val="0.95"/>
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
        <dia:point val="1.04753,2.52604"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.02753,2.50604;20.0627,17.6328"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="1.04753,2.52604"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="18.9952"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="15.0868"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.04"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O1">
      <dia:attribute name="obj_pos">
        <dia:point val="1.03885,3.2431"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.03385,3.2381;20.0453,3.2481"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.03885,3.2431"/>
        <dia:point val="20.0403,3.2431"/>
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
        <dia:point val="5.62885,2.51257"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.62258,2.50757;5.63385,17.6429"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.62885,2.51257"/>
        <dia:point val="5.62758,17.6379"/>
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
        <dia:point val="3.05844,3.2386"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.045,3.2336;3.06344,17.6363"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.05844,3.2386"/>
        <dia:point val="3.05,17.6313"/>
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
        <dia:point val="4.14436,3.2477"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.13936,3.2427;4.15758,17.6429"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.14436,3.2477"/>
        <dia:point val="4.15258,17.6379"/>
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
        <dia:point val="4.9062,3.2373"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.89758,3.2323;4.9112,17.6429"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.9062,3.2373"/>
        <dia:point val="4.90258,17.6379"/>
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
        <dia:point val="3.05844,3.9609"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.05344,3.9503;20.0561,3.9659"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.05844,3.9609"/>
        <dia:point val="20.0511,3.9553"/>
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
        <dia:point val="1.05258,4.65236"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.04758,4.64736;20.0448,4.6675"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.05258,4.65236"/>
        <dia:point val="20.0398,4.6625"/>
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
        <dia:point val="3.06703,5.3753"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.06203,5.3647;20.0647,5.3803"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.06703,5.3753"/>
        <dia:point val="20.0597,5.3697"/>
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
        <dia:point val="1.05258,6.10236"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.04758,6.0896;20.0693,6.10736"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.05258,6.10236"/>
        <dia:point val="20.0643,6.0946"/>
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
        <dia:point val="3.06703,6.8102"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.06203,6.7996;20.0647,6.8152"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.06703,6.8102"/>
        <dia:point val="20.0597,6.8046"/>
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
        <dia:point val="1.05258,7.50236"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.04758,7.49736;20.0467,7.50855"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.05258,7.50236"/>
        <dia:point val="20.0417,7.50355"/>
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
        <dia:point val="3.06795,8.2423"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.06295,8.2317;20.0656,8.2473"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.06795,8.2423"/>
        <dia:point val="20.0606,8.2367"/>
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
        <dia:point val="1.04562,8.9849"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.04062,8.9743;20.0702,8.9899"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.04562,8.9849"/>
        <dia:point val="20.0652,8.9793"/>
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
        <dia:point val="3.06795,9.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.06295,9.6894;20.0656,9.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.06795,9.7"/>
        <dia:point val="20.0606,9.6944"/>
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
        <dia:point val="1.05258,10.4077"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.04758,10.4027;20.0451,10.424"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.05258,10.4077"/>
        <dia:point val="20.0401,10.419"/>
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
        <dia:point val="3.06795,11.1498"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.06295,11.1392;20.0656,11.1548"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.06795,11.1498"/>
        <dia:point val="20.0606,11.1442"/>
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
        <dia:point val="1.02758,11.8827"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.02258,11.8777;20.0645,11.8918"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.02758,11.8827"/>
        <dia:point val="20.0595,11.8868"/>
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
        <dia:point val="3.0623,12.5819"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.0573,12.5713;20.06,12.5869"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.0623,12.5819"/>
        <dia:point val="20.055,12.5763"/>
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
        <dia:point val="1.05258,13.3577"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.04758,13.2962;20.0645,13.3627"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.05258,13.3577"/>
        <dia:point val="20.0595,13.3012"/>
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
        <dia:point val="3.0623,14.0271"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.0573,14.0165;20.06,14.0321"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.0623,14.0271"/>
        <dia:point val="20.055,14.0215"/>
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
        <dia:point val="1.05258,14.7327"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.04758,14.7237;20.0645,14.7377"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.05258,14.7327"/>
        <dia:point val="20.0595,14.7287"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O22">
      <dia:attribute name="obj_pos">
        <dia:point val="3.04462,15.4592"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.03962,15.4486;20.0423,15.4642"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.04462,15.4592"/>
        <dia:point val="20.0373,15.4536"/>
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
        <dia:point val="1.04161,16.1928"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.03661,16.1878;20.0485,16.198"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.04161,16.1928"/>
        <dia:point val="20.0435,16.193"/>
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
        <dia:point val="3.04625,16.908"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.04125,16.8973;20.0439,16.913"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="3.04625,16.908"/>
        <dia:point val="20.0389,16.9023"/>
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
        <dia:point val="12.3925,2.51578"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.3875,2.51078;12.3981,3.24078"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.3925,2.51578"/>
        <dia:point val="12.3931,3.23578"/>
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
        <dia:point val="7.67758,2.54266"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="7.67258,2.53766;7.68258,17.6429"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="7.67758,2.54266"/>
        <dia:point val="7.67758,17.6379"/>
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
        <dia:point val="11.0179,2.51244"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.0129,2.50744;11.0326,17.6429"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.0179,2.51244"/>
        <dia:point val="11.0276,17.6379"/>
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
        <dia:point val="12.3928,2.87578"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.3878,2.87078;20.047,2.89434"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.3928,2.87578"/>
        <dia:point val="20.042,2.88934"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O25" connection="0"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O29">
      <dia:attribute name="obj_pos">
        <dia:point val="11.0116,4.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.0066,3.9575;20.0484,4.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.0116,4.65"/>
        <dia:point val="20.0434,3.9625"/>
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
        <dia:point val="11.0116,6.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.0066,5.36105;20.0489,6.105"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.0116,6.1"/>
        <dia:point val="20.0439,5.36605"/>
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
        <dia:point val="11.0116,7.5"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.0066,6.795;20.0484,7.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.0116,7.5"/>
        <dia:point val="20.0434,6.8"/>
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
        <dia:point val="11.0116,8.975"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.0066,8.2298;20.0489,8.98"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.0116,8.975"/>
        <dia:point val="20.0439,8.2348"/>
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
        <dia:point val="11.0366,10.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.0316,9.6881;20.0184,10.405"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.0366,10.4"/>
        <dia:point val="20.0134,9.6931"/>
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
        <dia:point val="11.0366,11.875"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.0316,11.1406;20.0117,11.88"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.0366,11.875"/>
        <dia:point val="20.0067,11.1456"/>
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
        <dia:point val="11.0366,13.325"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.0316,12.5904;20.0293,13.33"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.0366,13.325"/>
        <dia:point val="20.0243,12.5954"/>
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
        <dia:point val="11.0366,14.725"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.0316,14.02;20.0489,14.73"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.0366,14.725"/>
        <dia:point val="20.0439,14.025"/>
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
        <dia:point val="11.0366,16.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.0316,15.455;20.0117,16.205"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.0366,16.2"/>
        <dia:point val="20.0067,15.46"/>
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
        <dia:point val="11.0219,17.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.0169,16.895;20.0489,17.605"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.0219,17.6"/>
        <dia:point val="20.0439,16.9"/>
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
        <dia:point val="14.0112,2.88958"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0062,2.88458;14.0162,3.9554"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="14.0112,2.88958"/>
        <dia:point val="14.0112,3.9504"/>
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
        <dia:point val="17.199,2.90166"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.194,2.89666;17.204,3.9675"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.199,2.90166"/>
        <dia:point val="17.199,3.9625"/>
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
        <dia:point val="14.0085,4.6634"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.0035,4.6584;14.0135,5.3756"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="14.0085,4.6634"/>
        <dia:point val="14.0085,5.3706"/>
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
        <dia:point val="17.2002,4.6634"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1952,4.6584;17.2052,5.3756"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.2002,4.6634"/>
        <dia:point val="17.2002,5.3706"/>
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
        <dia:point val="14.0047,6.101"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9997,6.096;14.0097,6.8132"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="14.0047,6.101"/>
        <dia:point val="14.0047,6.8082"/>
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
        <dia:point val="17.1964,6.101"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1914,6.096;17.2014,6.8132"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.1964,6.101"/>
        <dia:point val="17.1964,6.8082"/>
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
        <dia:point val="13.9693,7.517"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9643,7.512;13.9743,8.2292"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9693,7.517"/>
        <dia:point val="13.9693,8.2242"/>
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
        <dia:point val="17.186,7.517"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.181,7.512;17.1951,8.23563"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.186,7.517"/>
        <dia:point val="17.1901,8.23063"/>
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
        <dia:point val="13.987,8.9844"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.982,8.9794;13.9958,9.7066"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.987,8.9844"/>
        <dia:point val="13.9908,9.7016"/>
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
        <dia:point val="17.1825,8.9928"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1775,8.9878;17.1875,9.705"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.1825,8.9928"/>
        <dia:point val="17.1825,9.7"/>
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
        <dia:point val="13.987,10.435"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.982,10.43;13.9958,11.1571"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.987,10.435"/>
        <dia:point val="13.9908,11.1521"/>
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
        <dia:point val="17.1776,10.4181"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1726,10.4131;17.1875,11.1556"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.1776,10.4181"/>
        <dia:point val="17.1825,11.1506"/>
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
        <dia:point val="13.987,11.8917"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.982,11.8867;13.9951,12.5981"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.987,11.8917"/>
        <dia:point val="13.9901,12.5931"/>
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
        <dia:point val="17.1776,11.8806"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.1726,11.8756;17.1875,12.5829"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.1776,11.8806"/>
        <dia:point val="17.1825,12.5779"/>
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
        <dia:point val="13.9776,13.3181"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9726,13.3131;13.9873,14.0325"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9776,13.3181"/>
        <dia:point val="13.9823,14.0275"/>
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
        <dia:point val="17.1757,13.3179"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.169,13.3129;17.1807,14.0274"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.1757,13.3179"/>
        <dia:point val="17.174,14.0224"/>
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
        <dia:point val="13.9802,14.7468"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9726,14.7418;13.9852,15.4606"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9802,14.7468"/>
        <dia:point val="13.9776,15.4556"/>
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
        <dia:point val="17.1776,14.7244"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.169,14.7194;17.1826,15.4647"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.1776,14.7244"/>
        <dia:point val="17.174,15.4597"/>
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
        <dia:point val="13.9802,16.1966"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.9752,16.1916;13.9873,16.9321"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.9802,16.1966"/>
        <dia:point val="13.9823,16.9271"/>
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
        <dia:point val="17.1757,16.205"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.169,16.2;17.1807,16.9144"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="17.1757,16.205"/>
        <dia:point val="17.174,16.9094"/>
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
        <dia:point val="12.3781,16.1883"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.3731,16.1833;12.3853,16.9231"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.3781,16.1883"/>
        <dia:point val="12.3803,16.9181"/>
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
        <dia:point val="12.3781,14.7258"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.3731,14.7208;12.3853,15.4606"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.3781,14.7258"/>
        <dia:point val="12.3803,15.4556"/>
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
        <dia:point val="12.3803,13.3244"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.3731,13.3194;12.3853,14.0255"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.3803,13.3244"/>
        <dia:point val="12.3781,14.0205"/>
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
        <dia:point val="12.3781,11.8883"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.3731,11.8833;12.3853,12.5919"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.3781,11.8883"/>
        <dia:point val="12.3803,12.5869"/>
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
        <dia:point val="12.3803,10.4181"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.3731,10.4131;12.3853,11.1505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.3803,10.4181"/>
        <dia:point val="12.3781,11.1455"/>
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
        <dia:point val="12.3781,8.98828"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.3731,8.98328;12.3831,9.70048"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.3781,8.98828"/>
        <dia:point val="12.3781,9.69548"/>
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
        <dia:point val="12.3781,7.51328"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.3731,7.50828;12.3853,8.23563"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.3781,7.51328"/>
        <dia:point val="12.3803,8.23063"/>
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
        <dia:point val="12.3745,6.09938"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.3695,6.09438;12.3853,6.81063"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.3745,6.09938"/>
        <dia:point val="12.3803,6.80563"/>
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
        <dia:point val="12.3781,4.67364"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.3731,4.66864;12.3831,5.38584"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.3781,4.67364"/>
        <dia:point val="12.3781,5.38084"/>
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
        <dia:point val="12.3906,3.23828"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.3856,3.23328;12.3956,3.95048"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.3906,3.23828"/>
        <dia:point val="12.3906,3.94548"/>
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
        <dia:point val="11.7031,8.97127"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.6981,8.96627;11.7103,9.70438"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.7031,8.97127"/>
        <dia:point val="11.7053,9.69938"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O70">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7031,7.49627"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.6981,7.49127;11.7103,8.24188"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.7031,7.49627"/>
        <dia:point val="11.7053,8.23688"/>
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
        <dia:point val="11.7031,6.10877"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.6981,6.10377;11.7081,6.82097"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.7031,6.10877"/>
        <dia:point val="11.7031,6.81597"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O72">
      <dia:attribute name="obj_pos">
        <dia:point val="11.7281,4.66761"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.7231,4.66261;11.7331,5.37981"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.7281,4.66761"/>
        <dia:point val="11.7281,5.37481"/>
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
        <dia:point val="11.7281,3.23377"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.7195,3.22877;11.7331,3.95438"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.7281,3.23377"/>
        <dia:point val="11.7245,3.94938"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O74">
      <dia:attribute name="obj_pos">
        <dia:point val="11.6906,16.1988"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.6856,16.1938;11.6956,16.911"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.6906,16.1988"/>
        <dia:point val="11.6906,16.906"/>
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
        <dia:point val="11.6781,14.7488"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.6731,14.7438;11.6831,15.461"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.6781,14.7488"/>
        <dia:point val="11.6781,15.456"/>
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
        <dia:point val="11.7031,13.3363"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.6981,13.3313;11.7103,14.0294"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.7031,13.3363"/>
        <dia:point val="11.7053,14.0244"/>
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
        <dia:point val="11.6781,11.8988"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.6731,11.8938;11.6853,12.5919"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.6781,11.8988"/>
        <dia:point val="11.6803,12.5869"/>
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
        <dia:point val="11.6803,10.4181"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.6731,10.4131;11.6853,11.161"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="11.6803,10.4181"/>
        <dia:point val="11.6781,11.156"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:group>
      <dia:object type="Standard - Line" version="0" id="O79">
        <dia:attribute name="obj_pos">
          <dia:point val="1.04307,18.2286"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="1.03307,18.2186;6.62807,18.2386"/>
        </dia:attribute>
        <dia:attribute name="conn_endpoints">
          <dia:point val="1.04307,18.2286"/>
          <dia:point val="6.61807,18.2286"/>
        </dia:attribute>
        <dia:attribute name="numcp">
          <dia:int val="1"/>
        </dia:attribute>
        <dia:attribute name="line_width">
          <dia:real val="0.02"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Line" version="0" id="O80">
        <dia:attribute name="obj_pos">
          <dia:point val="7.81807,18.2286"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="7.80807,18.2186;14.0281,18.2386"/>
        </dia:attribute>
        <dia:attribute name="conn_endpoints">
          <dia:point val="7.81807,18.2286"/>
          <dia:point val="14.0181,18.2286"/>
        </dia:attribute>
        <dia:attribute name="numcp">
          <dia:int val="1"/>
        </dia:attribute>
        <dia:attribute name="line_width">
          <dia:real val="0.02"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Line" version="0" id="O81">
        <dia:attribute name="obj_pos">
          <dia:point val="15.0181,18.2286"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="15.0081,18.2186;20.0281,18.2386"/>
        </dia:attribute>
        <dia:attribute name="conn_endpoints">
          <dia:point val="15.0181,18.2286"/>
          <dia:point val="20.0181,18.2286"/>
        </dia:attribute>
        <dia:attribute name="numcp">
          <dia:int val="1"/>
        </dia:attribute>
        <dia:attribute name="line_width">
          <dia:real val="0.02"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Line" version="0" id="O82">
        <dia:attribute name="obj_pos">
          <dia:point val="1.00807,18.3111"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.99807,18.3011;6.59307,18.3211"/>
        </dia:attribute>
        <dia:attribute name="conn_endpoints">
          <dia:point val="1.00807,18.3111"/>
          <dia:point val="6.58307,18.3111"/>
        </dia:attribute>
        <dia:attribute name="numcp">
          <dia:int val="1"/>
        </dia:attribute>
        <dia:attribute name="line_width">
          <dia:real val="0.02"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Line" version="0" id="O83">
        <dia:attribute name="obj_pos">
          <dia:point val="7.81807,18.3036"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="7.80807,18.2936;14.0681,18.3211"/>
        </dia:attribute>
        <dia:attribute name="conn_endpoints">
          <dia:point val="7.81807,18.3036"/>
          <dia:point val="14.0581,18.3111"/>
        </dia:attribute>
        <dia:attribute name="numcp">
          <dia:int val="1"/>
        </dia:attribute>
        <dia:attribute name="line_width">
          <dia:real val="0.02"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Line" version="0" id="O84">
        <dia:attribute name="obj_pos">
          <dia:point val="15.0081,18.3111"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="14.9981,18.3011;20.0181,18.3211"/>
        </dia:attribute>
        <dia:attribute name="conn_endpoints">
          <dia:point val="15.0081,18.3111"/>
          <dia:point val="20.0081,18.3111"/>
        </dia:attribute>
        <dia:attribute name="numcp">
          <dia:int val="1"/>
        </dia:attribute>
        <dia:attribute name="line_width">
          <dia:real val="0.02"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Standard - Text" version="0" id="O85">
        <dia:attribute name="obj_pos">
          <dia:point val="7.04307,18.3786"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="7.04307,18.0601;7.44707,18.4601"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#○#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.500000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.04307,18.3786"/>
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
      <dia:object type="Standard - Text" version="0" id="O86">
        <dia:attribute name="obj_pos">
          <dia:point val="14.3431,18.3786"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="14.3431,18.0601;14.7471,18.4601"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#○#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.500000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="14.3431,18.3786"/>
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
    </dia:group>
    <dia:object type="Standard - Text" version="0" id="O87">
      <dia:attribute name="obj_pos">
        <dia:point val="1.06807,18.8036"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.06807,18.5249;14.5011,18.8749"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※在宅時医学総合管理又は在宅末期医療総合診療の請求分は別掲で記載して下さい。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.06807,18.8036"/>
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
    <dia:object type="Standard - Box" version="0" id="O88">
      <dia:attribute name="obj_pos">
        <dia:point val="1.01807,19.1609"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.99807,19.1409;15.0381,26.5024"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="1.01807,19.1609"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="14"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="7.32148"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.04"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O89">
      <dia:attribute name="obj_pos">
        <dia:point val="1.04307,20.0609"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.03807,20.0559;15.0231,20.0659"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.04307,20.0609"/>
        <dia:point val="15.0181,20.0609"/>
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
        <dia:point val="1.01807,21.7359"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.01307,21.7309;15.0289,21.7425"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.01807,21.7359"/>
        <dia:point val="15.0239,21.7375"/>
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
        <dia:point val="1.04307,23.7074"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.03807,23.7024;15.0231,23.7159"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.04307,23.7074"/>
        <dia:point val="15.0181,23.7109"/>
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
        <dia:point val="1.01807,25.6359"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.01307,25.6274;15.0231,25.6409"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.01807,25.6359"/>
        <dia:point val="15.0181,25.6324"/>
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
        <dia:point val="2.76807,20.0574"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.7575,20.0524;2.77307,25.655"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.76807,20.0574"/>
        <dia:point val="2.7625,25.65"/>
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
        <dia:point val="5.63958,19.1625"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.63458,19.1575;5.64458,26.4925"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.63958,19.1625"/>
        <dia:point val="5.63958,26.4875"/>
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
        <dia:point val="12.0431,19.1824"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.0381,19.1774;12.0481,26.5124"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="12.0431,19.1824"/>
        <dia:point val="12.0431,26.5074"/>
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
        <dia:point val="5.64307,19.6074"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.63807,19.6024;12.0493,19.6175"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.64307,19.6074"/>
        <dia:point val="12.0443,19.6125"/>
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
        <dia:point val="8.53057,19.6074"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.52557,19.6024;8.53681,26.505"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.53057,19.6074"/>
        <dia:point val="8.53181,26.5"/>
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
        <dia:point val="2.76576,20.9375"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.76076,20.9325;15.0174,20.9425"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.76576,20.9375"/>
        <dia:point val="15.0124,20.9375"/>
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
        <dia:point val="2.76807,22.6824"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.76307,22.6774;15.0231,22.6874"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.76807,22.6824"/>
        <dia:point val="15.0181,22.6824"/>
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
        <dia:point val="2.76807,24.6574"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.76307,24.6524;15.0198,24.6675"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="2.76807,24.6574"/>
        <dia:point val="15.0148,24.6625"/>
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
        <dia:point val="4.31807,20.0574"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.31307,20.0524;4.32307,25.6374"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.31807,20.0574"/>
        <dia:point val="4.31807,25.6324"/>
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
        <dia:point val="5.01807,20.0574"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.01307,20.0524;5.02307,25.6374"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.01807,20.0574"/>
        <dia:point val="5.01807,25.6324"/>
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
        <dia:point val="13.6981,2.2948"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.6931,2.2898;20.0281,2.2998"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="13.6981,2.2948"/>
        <dia:point val="20.0231,2.2948"/>
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
    <dia:object type="Standard - Text" version="0" id="O104">
      <dia:attribute name="obj_pos">
        <dia:point val="3.20258,3.77445"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.20258,3.53556;3.99038,3.83556"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入 院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.20258,3.77445"/>
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
    <dia:object type="Standard - Text" version="0" id="O105">
      <dia:attribute name="obj_pos">
        <dia:point val="3.19786,4.4475"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.19786,4.20861;3.98566,4.50861"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#外 来#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.19786,4.4475"/>
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
    <dia:object type="Standard - Text" version="0" id="O106">
      <dia:attribute name="obj_pos">
        <dia:point val="3.19811,5.1894"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.19811,4.95051;3.98591,5.25051"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入 院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.19811,5.1894"/>
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
    <dia:object type="Standard - Text" version="0" id="O107">
      <dia:attribute name="obj_pos">
        <dia:point val="3.20329,5.8781"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.20329,5.63921;3.99109,5.93921"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#外 来#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.20329,5.8781"/>
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
    <dia:object type="Standard - Text" version="0" id="O108">
      <dia:attribute name="obj_pos">
        <dia:point val="3.18561,6.6021"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.18561,6.36321;3.97341,6.66321"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入 院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.18561,6.6021"/>
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
    <dia:object type="Standard - Text" version="0" id="O109">
      <dia:attribute name="obj_pos">
        <dia:point val="3.19079,7.2908"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.19079,7.05191;3.97859,7.35191"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#外 来#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.19079,7.2908"/>
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
    <dia:object type="Standard - Text" version="0" id="O110">
      <dia:attribute name="obj_pos">
        <dia:point val="3.19811,8.0271"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.19811,7.78821;3.98591,8.08821"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入 院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.19811,8.0271"/>
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
    <dia:object type="Standard - Text" version="0" id="O111">
      <dia:attribute name="obj_pos">
        <dia:point val="3.20329,8.7657"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.20329,8.52681;3.99109,8.82681"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#外 来#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.20329,8.7657"/>
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
    <dia:object type="Standard - Text" version="0" id="O112">
      <dia:attribute name="obj_pos">
        <dia:point val="3.19811,9.5023"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.19811,9.26341;3.98591,9.56341"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入 院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.19811,9.5023"/>
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
    <dia:object type="Standard - Text" version="0" id="O113">
      <dia:attribute name="obj_pos">
        <dia:point val="3.20329,10.1909"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.20329,9.95201;3.99109,10.252"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#外 来#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.20329,10.1909"/>
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
    <dia:object type="Standard - Text" version="0" id="O114">
      <dia:attribute name="obj_pos">
        <dia:point val="3.20719,10.9275"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.20719,10.6886;3.99499,10.9886"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入 院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.20719,10.9275"/>
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
    <dia:object type="Standard - Text" version="0" id="O115">
      <dia:attribute name="obj_pos">
        <dia:point val="3.21237,11.6161"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.21237,11.3772;4.00017,11.6772"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#外 来#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.21237,11.6161"/>
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
    <dia:object type="Standard - Text" version="0" id="O116">
      <dia:attribute name="obj_pos">
        <dia:point val="3.20719,12.3714"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.20719,12.1325;3.99499,12.4325"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入 院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.20719,12.3714"/>
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
    <dia:object type="Standard - Text" version="0" id="O117">
      <dia:attribute name="obj_pos">
        <dia:point val="3.21237,13.0601"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.21237,12.8212;4.00017,13.1212"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#外 来#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.21237,13.0601"/>
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
    <dia:object type="Standard - Text" version="0" id="O118">
      <dia:attribute name="obj_pos">
        <dia:point val="3.20719,13.8091"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.20719,13.5702;3.99499,13.8702"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入 院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.20719,13.8091"/>
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
    <dia:object type="Standard - Text" version="0" id="O119">
      <dia:attribute name="obj_pos">
        <dia:point val="3.21237,14.4978"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.21237,14.2589;4.00017,14.5589"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#外 来#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.21237,14.4978"/>
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
    <dia:object type="Standard - Text" version="0" id="O120">
      <dia:attribute name="obj_pos">
        <dia:point val="3.20719,15.2343"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.20719,14.9954;3.99499,15.2954"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入 院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.20719,15.2343"/>
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
    <dia:object type="Standard - Text" version="0" id="O121">
      <dia:attribute name="obj_pos">
        <dia:point val="3.21237,15.923"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.21237,15.6841;4.00017,15.9841"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#外 来#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.21237,15.923"/>
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
    <dia:object type="Standard - Text" version="0" id="O122">
      <dia:attribute name="obj_pos">
        <dia:point val="3.20719,16.7039"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.20719,16.465;3.99499,16.765"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#入 院#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.20719,16.7039"/>
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
    <dia:object type="Standard - Text" version="0" id="O123">
      <dia:attribute name="obj_pos">
        <dia:point val="3.21237,17.3925"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.21237,17.1536;4.00017,17.4536"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#外 来#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.21237,17.3925"/>
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
    <dia:object type="Standard - Text" version="0" id="O124">
      <dia:attribute name="obj_pos">
        <dia:point val="15.6259,2.82618"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.2624,2.58729;16.9894,2.88729"/>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.6259,2.82618"/>
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
    <dia:object type="Standard - Text" version="0" id="O125">
      <dia:attribute name="obj_pos">
        <dia:point val="12.8372,3.1975"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.8372,2.95861;13.4432,3.25861"/>
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
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.8372,3.1975"/>
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
    <dia:object type="Standard - Text" version="0" id="O126">
      <dia:attribute name="obj_pos">
        <dia:point val="15.5973,3.1975"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.9307,2.95861;16.2639,3.25861"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#金    額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.5973,3.1975"/>
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
    <dia:object type="Standard - Text" version="0" id="O127">
      <dia:attribute name="obj_pos">
        <dia:point val="18.6957,3.1922"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="17.9382,2.95331;19.4532,3.25331"/>
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
            <dia:point val="18.6957,3.1922"/>
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
    <dia:object type="Standard - Text" version="0" id="O128">
      <dia:attribute name="obj_pos">
        <dia:point val="4.99307,3.73679"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.99307,3.45809;5.41727,3.80809"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.99307,3.73679"/>
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
        <dia:point val="1.89307,3.10044"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.89307,2.82174;5.07457,3.17174"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#公 費 負 担 医 療#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.89307,3.10044"/>
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
    <dia:object type="Standard - Text" version="0" id="O130">
      <dia:attribute name="obj_pos">
        <dia:point val="6.31807,3.07544"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.31807,2.83655;7.10587,3.13655"/>
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
            <dia:point val="6.31807,3.07544"/>
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
    <dia:object type="Standard - Text" version="0" id="O131">
      <dia:attribute name="obj_pos">
        <dia:point val="8.4181,3.10044"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.4181,2.86155;10.6603,3.16155"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#患 者 負 担 額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.4181,3.10044"/>
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
    <dia:object type="Standard - Text" version="0" id="O132">
      <dia:attribute name="obj_pos">
        <dia:point val="11.0431,2.95044"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="11.0431,2.76729;12.4369,2.99729"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#公費負担医療#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.287500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.0431,2.95044"/>
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
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O133">
      <dia:attribute name="obj_pos">
        <dia:point val="2.54307,19.6074"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.54307,19.3685;4.42167,19.6685"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#区       分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.54307,19.6074"/>
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
    <dia:object type="Standard - Text" version="0" id="O134">
      <dia:attribute name="obj_pos">
        <dia:point val="2.54307,19.9574"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.54307,19.7185;4.30047,20.0185"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#法 制 区 分#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.54307,19.9574"/>
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
    <dia:object type="Standard - Text" version="0" id="O135">
      <dia:attribute name="obj_pos">
        <dia:point val="8.04307,19.5074"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.04307,19.2287;9.81057,19.5787"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#療養の給付#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="8.04307,19.5074"/>
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
    <dia:object type="Standard - Text" version="0" id="O136">
      <dia:attribute name="obj_pos">
        <dia:point val="6.41807,19.9824"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.41807,19.7435;7.75127,20.0435"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件    数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.41807,19.9824"/>
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
    <dia:object type="Standard - Text" version="0" id="O137">
      <dia:attribute name="obj_pos">
        <dia:point val="9.66807,19.9824"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.66807,19.7435;11.0013,20.0435"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#点    数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.66807,19.9824"/>
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
    <dia:object type="Standard - Text" version="0" id="O138">
      <dia:attribute name="obj_pos">
        <dia:point val="12.5431,19.7324"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.5431,19.4935;14.7853,19.7935"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#一 部 負 担 金#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="12.5431,19.7324"/>
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
    <dia:object type="Standard - Text" version="0" id="O139">
      <dia:attribute name="obj_pos">
        <dia:point val="1.19307,20.8074"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.19307,20.4093;1.69807,21.4093"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#国
保#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.19307,20.8074"/>
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
    <dia:object type="Standard - Text" version="0" id="O140">
      <dia:attribute name="obj_pos">
        <dia:point val="1.19807,24.5708"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.19807,24.1727;1.70307,25.1727"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#老
人#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.19807,24.5708"/>
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
    <dia:object type="Standard - Text" version="0" id="O141">
      <dia:attribute name="obj_pos">
        <dia:point val="1.19307,22.6074"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.19307,22.2093;1.69807,23.2093"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#退
職#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.19307,22.6074"/>
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
    <dia:object type="ORCA - TextCircle" version="0" id="O142">
      <dia:attribute name="obj_pos">
        <dia:point val="2.01807,21.0574"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.01807,20.7389;2.42207,21.1389"/>
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
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.01807,21.0574"/>
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
    <dia:object type="ORCA - TextCircle" version="0" id="O143">
      <dia:attribute name="obj_pos">
        <dia:point val="2.02307,24.8176"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.02307,24.4991;2.42707,24.8991"/>
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
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.02307,24.8176"/>
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
    <dia:object type="ORCA - TextCircle" version="0" id="O144">
      <dia:attribute name="obj_pos">
        <dia:point val="2.01807,22.8574"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.01807,22.5389;2.42207,22.9389"/>
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
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.01807,22.8574"/>
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
    <dia:object type="Standard - Text" version="0" id="O145">
      <dia:attribute name="obj_pos">
        <dia:point val="3.01807,20.4282"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.01807,20.1893;4.23007,20.7893"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#７０以上
  ９割#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.01807,20.4282"/>
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
    <dia:object type="Standard - Text" version="0" id="O146">
      <dia:attribute name="obj_pos">
        <dia:point val="2.99307,22.2282"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.99307,21.9893;4.20507,22.5893"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#７０以上
  ９割#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.99307,22.2282"/>
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
    <dia:object type="Standard - Text" version="0" id="O147">
      <dia:attribute name="obj_pos">
        <dia:point val="2.99307,21.3282"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.99307,21.0893;4.20507,21.6893"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#７０以上
  ７割#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.99307,21.3282"/>
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
    <dia:object type="Standard - Text" version="0" id="O148">
      <dia:attribute name="obj_pos">
        <dia:point val="2.99307,23.1532"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.99307,22.9143;4.20507,23.5143"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#７０以上
  ７割#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.99307,23.1532"/>
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
    <dia:object type="Standard - Text" version="0" id="O149">
      <dia:attribute name="obj_pos">
        <dia:point val="2.99307,24.3032"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.99307,24.0643;3.96267,24.3643"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#  ９割#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.99307,24.3032"/>
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
    <dia:object type="Standard - Text" version="0" id="O150">
      <dia:attribute name="obj_pos">
        <dia:point val="2.99307,25.2282"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.99307,24.9893;3.96267,25.2893"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#  ７割#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.99307,25.2282"/>
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
    <dia:object type="Standard - Text" version="0" id="O151">
      <dia:attribute name="obj_pos">
        <dia:point val="2.26807,26.2532"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.26807,25.9347;4.28807,26.3347"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#合     計#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.500000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.26807,26.2532"/>
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
    <dia:object type="Standard - Text" version="0" id="O152">
      <dia:attribute name="obj_pos">
        <dia:point val="4.49307,20.6282"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.49307,20.3893;4.85667,20.6893"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#25#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.49307,20.6282"/>
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
    <dia:object type="Standard - Text" version="0" id="O153">
      <dia:attribute name="obj_pos">
        <dia:point val="4.49307,21.4032"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.49307,21.1643;4.85667,21.4643"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#25#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.49307,21.4032"/>
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
    <dia:object type="Standard - Text" version="0" id="O154">
      <dia:attribute name="obj_pos">
        <dia:point val="4.49307,22.3032"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.49307,22.0643;4.85667,22.3643"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#67#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.49307,22.3032"/>
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
    <dia:object type="Standard - Text" version="0" id="O155">
      <dia:attribute name="obj_pos">
        <dia:point val="4.49307,23.3032"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.49307,23.0643;4.85667,23.3643"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#67#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.49307,23.3032"/>
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
    <dia:object type="Standard - Text" version="0" id="O156">
      <dia:attribute name="obj_pos">
        <dia:point val="4.46807,24.2782"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.46807,24.0393;4.83167,24.3393"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#27#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.46807,24.2782"/>
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
    <dia:object type="Standard - Text" version="0" id="O157">
      <dia:attribute name="obj_pos">
        <dia:point val="4.46807,25.2032"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.46807,24.9643;4.83167,25.2643"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#27#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.46807,25.2032"/>
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
    <dia:object type="Standard - Text" version="0" id="O158">
      <dia:attribute name="obj_pos">
        <dia:point val="5.09307,20.6532"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09307,20.4143;5.63847,20.7143"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#039#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.09307,20.6532"/>
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
    <dia:object type="Standard - Text" version="0" id="O159">
      <dia:attribute name="obj_pos">
        <dia:point val="5.09307,22.3032"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09307,22.0643;5.63847,22.3643"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#139#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.09307,22.3032"/>
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
    <dia:object type="Standard - Text" version="0" id="O160">
      <dia:attribute name="obj_pos">
        <dia:point val="5.09307,24.2532"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09307,24.0143;5.63847,24.3143"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#339#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.09307,24.2532"/>
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
    <dia:object type="Standard - Text" version="0" id="O161">
      <dia:attribute name="obj_pos">
        <dia:point val="5.09307,21.4032"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09307,21.1643;5.63847,21.4643"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#038#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.09307,21.4032"/>
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
    <dia:object type="Standard - Text" version="0" id="O162">
      <dia:attribute name="obj_pos">
        <dia:point val="5.09307,23.3032"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.09307,23.0643;5.63847,23.3643"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#138#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.09307,23.3032"/>
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
    <dia:object type="Standard - Text" version="0" id="O163">
      <dia:attribute name="obj_pos">
        <dia:point val="5.06807,25.2282"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.06807,24.9893;5.61347,25.2893"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#338#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.06807,25.2282"/>
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
    <dia:object type="Standard - Box" version="0" id="O164">
      <dia:attribute name="obj_pos">
        <dia:point val="0.993069,27.0319"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.973069,27.0119;11.9881,29.9769"/>
      </dia:attribute>
      <dia:attribute name="elem_corner">
        <dia:point val="0.993069,27.0319"/>
      </dia:attribute>
      <dia:attribute name="elem_width">
        <dia:real val="10.975"/>
      </dia:attribute>
      <dia:attribute name="elem_height">
        <dia:real val="2.925"/>
      </dia:attribute>
      <dia:attribute name="border_width">
        <dia:real val="0.04"/>
      </dia:attribute>
      <dia:attribute name="show_background">
        <dia:boolean val="false"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O165">
      <dia:attribute name="obj_pos">
        <dia:point val="5.64307,27.0444"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.6325,27.0394;5.64807,29.9682"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="5.64307,27.0444"/>
        <dia:point val="5.6375,29.9632"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O166">
      <dia:attribute name="obj_pos">
        <dia:point val="1.01807,27.6819"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.01307,27.6769;11.9981,27.6869"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="1.01807,27.6819"/>
        <dia:point val="11.9931,27.6819"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O167">
      <dia:attribute name="obj_pos">
        <dia:point val="0.993069,28.4944"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.988069,28.4894;11.9731,28.4994"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="0.993069,28.4944"/>
        <dia:point val="11.9681,28.4944"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
      <dia:connections>
        <dia:connection handle="0" to="O164" connection="3"/>
        <dia:connection handle="1" to="O164" connection="4"/>
      </dia:connections>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O168">
      <dia:attribute name="obj_pos">
        <dia:point val="0.999439,29.1791"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.994439,29.1741;11.9981,29.1869"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="0.999439,29.1791"/>
        <dia:point val="11.9931,29.1819"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O169">
      <dia:attribute name="obj_pos">
        <dia:point val="8.51807,27.0569"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.51307,27.0519;8.52307,29.9619"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="8.51807,27.0569"/>
        <dia:point val="8.51807,29.9569"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O170">
      <dia:attribute name="obj_pos">
        <dia:point val="4.24307,27.6819"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.23807,27.6769;4.24807,29.9619"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.24307,27.6819"/>
        <dia:point val="4.24307,29.9569"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Line" version="0" id="O171">
      <dia:attribute name="obj_pos">
        <dia:point val="4.94307,27.6819"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.93807,27.6769;4.94807,29.9869"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="4.94307,27.6819"/>
        <dia:point val="4.94307,29.9819"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="1"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0.01"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="0" id="O172">
      <dia:attribute name="obj_pos">
        <dia:point val="2.41807,27.5069"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.41807,27.2282;4.53907,27.5782"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#公費負担医療#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.41807,27.5069"/>
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
    <dia:object type="Standard - Text" version="0" id="O173">
      <dia:attribute name="obj_pos">
        <dia:point val="6.41807,27.5069"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.41807,27.2282;7.97347,27.5782"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#件    数#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="6.41807,27.5069"/>
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
    <dia:object type="Standard - Text" version="0" id="O174">
      <dia:attribute name="obj_pos">
        <dia:point val="9.06807,27.4819"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.06807,27.2032;11.684,27.5532"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#患 者 負 担 額#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="9.06807,27.4819"/>
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
    <dia:object type="Standard - Text" version="0" id="O175">
      <dia:attribute name="obj_pos">
        <dia:point val="4.44307,28.1569"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.44307,27.918;4.80667,28.218"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#43#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.44307,28.1569"/>
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
    <dia:object type="Standard - Text" version="0" id="O176">
      <dia:attribute name="obj_pos">
        <dia:point val="4.46807,29.6569"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.46807,29.418;4.83167,29.718"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#43#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.46807,29.6569"/>
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
    <dia:object type="Standard - Text" version="0" id="O177">
      <dia:attribute name="obj_pos">
        <dia:point val="4.44307,28.9069"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.44307,28.668;4.80667,28.968"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#43#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.44307,28.9069"/>
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
    <dia:object type="Standard - Text" version="0" id="O178">
      <dia:attribute name="obj_pos">
        <dia:point val="5.04307,28.1319"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.04307,27.893;5.40667,28.193"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.04307,28.1319"/>
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
    <dia:object type="Standard - Text" version="0" id="O179">
      <dia:attribute name="obj_pos">
        <dia:point val="5.04807,29.639"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.04807,29.4001;5.41167,29.7001"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.04807,29.639"/>
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
    <dia:object type="Standard - Text" version="0" id="O180">
      <dia:attribute name="obj_pos">
        <dia:point val="5.04307,28.9319"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.04307,28.693;5.40667,28.993"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.04307,28.9319"/>
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
      <dia:object type="Standard - Text" version="0" id="O181">
        <dia:attribute name="obj_pos">
          <dia:point val="4.2186,3.7392"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.2186,3.50031;4.764,3.80031"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.2186,3.7392"/>
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
      <dia:object type="Standard - Text" version="0" id="O182">
        <dia:attribute name="obj_pos">
          <dia:point val="4.21985,4.4453"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.21985,4.20641;4.76525,4.50641"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 42#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.21985,4.4453"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O183">
        <dia:attribute name="obj_pos">
          <dia:point val="4.22307,5.15746"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.22307,4.91857;4.76847,5.21857"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.22307,5.15746"/>
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
      <dia:object type="Standard - Text" version="0" id="O184">
        <dia:attribute name="obj_pos">
          <dia:point val="4.22432,5.86356"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.22432,5.62467;4.76972,5.92467"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 42#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.22432,5.86356"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O185">
        <dia:attribute name="obj_pos">
          <dia:point val="4.22307,10.9325"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.22307,10.6936;4.76847,10.9936"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.22307,10.9325"/>
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
      <dia:object type="Standard - Text" version="0" id="O186">
        <dia:attribute name="obj_pos">
          <dia:point val="4.22432,11.6386"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.22432,11.3997;4.76972,11.6997"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 42#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.22432,11.6386"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O187">
        <dia:attribute name="obj_pos">
          <dia:point val="4.22307,9.45746"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.22307,9.21857;4.76847,9.51857"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.22307,9.45746"/>
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
      <dia:object type="Standard - Text" version="0" id="O188">
        <dia:attribute name="obj_pos">
          <dia:point val="4.22432,10.1636"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.22432,9.92471;4.76972,10.2247"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 42#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.22432,10.1636"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O189">
        <dia:attribute name="obj_pos">
          <dia:point val="4.19807,8.05746"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.19807,7.81857;4.74347,8.11857"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.19807,8.05746"/>
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
      <dia:object type="Standard - Text" version="0" id="O190">
        <dia:attribute name="obj_pos">
          <dia:point val="4.19932,8.76356"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.19932,8.52467;4.74472,8.82467"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 42#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.19932,8.76356"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O191">
        <dia:attribute name="obj_pos">
          <dia:point val="4.22307,6.58246"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.22307,6.34357;4.76847,6.64357"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.22307,6.58246"/>
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
      <dia:object type="Standard - Text" version="0" id="O192">
        <dia:attribute name="obj_pos">
          <dia:point val="4.22432,7.28856"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.22432,7.04967;4.76972,7.34967"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 42#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.22432,7.28856"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O193">
        <dia:attribute name="obj_pos">
          <dia:point val="4.22307,12.355"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.22307,12.1161;4.76847,12.4161"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.22307,12.355"/>
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
      <dia:object type="Standard - Text" version="0" id="O194">
        <dia:attribute name="obj_pos">
          <dia:point val="4.22432,13.0611"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.22432,12.8222;4.76972,13.1222"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 42#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.22432,13.0611"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O195">
        <dia:attribute name="obj_pos">
          <dia:point val="4.22307,13.83"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.22307,13.5911;4.76847,13.8911"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.22307,13.83"/>
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
      <dia:object type="Standard - Text" version="0" id="O196">
        <dia:attribute name="obj_pos">
          <dia:point val="4.22432,14.5361"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.22432,14.2972;4.76972,14.5972"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 42#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.22432,14.5361"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O197">
        <dia:attribute name="obj_pos">
          <dia:point val="4.19807,15.2388"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.19807,14.9999;4.74347,15.2999"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.19807,15.2388"/>
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
      <dia:object type="Standard - Text" version="0" id="O198">
        <dia:attribute name="obj_pos">
          <dia:point val="4.19932,15.9449"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.19932,15.706;4.74472,16.006"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 42#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.19932,15.9449"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O199">
        <dia:attribute name="obj_pos">
          <dia:point val="4.19807,16.6888"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.19807,16.4499;4.74347,16.7499"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.19807,16.6888"/>
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
      <dia:object type="Standard - Text" version="0" id="O200">
        <dia:attribute name="obj_pos">
          <dia:point val="4.19932,17.3949"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="4.19932,17.156;4.74472,17.456"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 42#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="4.19932,17.3949"/>
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
    </dia:group>
    <dia:object type="Standard - Text" version="0" id="O201">
      <dia:attribute name="obj_pos">
        <dia:point val="4.99307,4.46679"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.99307,4.18809;5.41727,4.53809"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.99307,4.46679"/>
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
    <dia:object type="Standard - Text" version="0" id="O202">
      <dia:attribute name="obj_pos">
        <dia:point val="5.01807,5.19179"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.01807,4.91309;5.44227,5.26309"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.01807,5.19179"/>
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
    <dia:object type="Standard - Text" version="0" id="O203">
      <dia:attribute name="obj_pos">
        <dia:point val="5.04807,13.0629"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.04807,12.7842;5.47227,13.1342"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.04807,13.0629"/>
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
    <dia:object type="Standard - Text" version="0" id="O204">
      <dia:attribute name="obj_pos">
        <dia:point val="5.04807,12.3379"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.04807,12.0592;5.47227,12.4092"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.04807,12.3379"/>
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
    <dia:object type="Standard - Text" version="0" id="O205">
      <dia:attribute name="obj_pos">
        <dia:point val="5.04807,11.6129"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.04807,11.3342;5.47227,11.6842"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.04807,11.6129"/>
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
    <dia:object type="Standard - Text" version="0" id="O206">
      <dia:attribute name="obj_pos">
        <dia:point val="5.04807,10.9379"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.04807,10.6592;5.47227,11.0092"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.04807,10.9379"/>
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
    <dia:object type="Standard - Text" version="0" id="O207">
      <dia:attribute name="obj_pos">
        <dia:point val="5.01807,10.2168"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.01807,9.9381;5.44227,10.2881"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.01807,10.2168"/>
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
    <dia:object type="Standard - Text" version="0" id="O208">
      <dia:attribute name="obj_pos">
        <dia:point val="5.04307,9.44179"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.04307,9.16309;5.46727,9.51309"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.04307,9.44179"/>
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
    <dia:object type="Standard - Text" version="0" id="O209">
      <dia:attribute name="obj_pos">
        <dia:point val="5.04307,8.74179"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.04307,8.46309;5.46727,8.81309"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.04307,8.74179"/>
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
    <dia:object type="Standard - Text" version="0" id="O210">
      <dia:attribute name="obj_pos">
        <dia:point val="5.02307,8.06293"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.02307,7.78423;5.44727,8.13423"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.02307,8.06293"/>
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
    <dia:object type="Standard - Text" version="0" id="O211">
      <dia:attribute name="obj_pos">
        <dia:point val="4.99807,7.31293"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.99807,7.03423;5.42227,7.38423"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.99807,7.31293"/>
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
    <dia:object type="Standard - Text" version="0" id="O212">
      <dia:attribute name="obj_pos">
        <dia:point val="4.99807,6.58793"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.99807,6.30923;5.42227,6.65923"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.99807,6.58793"/>
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
    <dia:object type="Standard - Text" version="0" id="O213">
      <dia:attribute name="obj_pos">
        <dia:point val="5.01807,5.89179"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.01807,5.61309;5.44227,5.96309"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.01807,5.89179"/>
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
    <dia:object type="Standard - Text" version="0" id="O214">
      <dia:attribute name="obj_pos">
        <dia:point val="5.01807,15.9468"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.01807,15.6681;5.44227,16.0181"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.01807,15.9468"/>
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
    <dia:object type="Standard - Text" version="0" id="O215">
      <dia:attribute name="obj_pos">
        <dia:point val="4.99307,15.2218"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.99307,14.9431;5.41727,15.2931"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.99307,15.2218"/>
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
    <dia:object type="Standard - Text" version="0" id="O216">
      <dia:attribute name="obj_pos">
        <dia:point val="4.99307,14.4968"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.99307,14.2181;5.41727,14.5681"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.99307,14.4968"/>
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
    <dia:object type="Standard - Text" version="0" id="O217">
      <dia:attribute name="obj_pos">
        <dia:point val="5.01807,13.7968"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="5.01807,13.5181;5.44227,13.8681"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="5.01807,13.7968"/>
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
    <dia:object type="Standard - Text" version="0" id="O218">
      <dia:attribute name="obj_pos">
        <dia:point val="4.99307,17.3968"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.99307,17.1181;5.41727,17.4681"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.99307,17.3968"/>
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
    <dia:object type="Standard - Text" version="0" id="O219">
      <dia:attribute name="obj_pos">
        <dia:point val="4.99307,16.6968"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="4.99307,16.4181;5.41727,16.7681"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string># 1#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="4.99307,16.6968"/>
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
      <dia:object type="Standard - Text" version="0" id="O220">
        <dia:attribute name="obj_pos">
          <dia:point val="11.1181,3.767"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.1181,3.52811;11.6635,3.82811"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.1181,3.767"/>
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
      <dia:object type="Standard - Text" version="0" id="O221">
        <dia:attribute name="obj_pos">
          <dia:point val="11.7681,3.75639"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.7681,3.47769;12.1923,3.82769"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 2#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.437500"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.7681,3.75639"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O222">
        <dia:attribute name="obj_pos">
          <dia:point val="11.1481,10.9631"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.1481,10.7242;11.6935,11.0242"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.1481,10.9631"/>
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
      <dia:object type="Standard - Text" version="0" id="O223">
        <dia:attribute name="obj_pos">
          <dia:point val="11.7981,10.9525"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.7981,10.6738;12.2223,11.0238"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 2#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.437500"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.7981,10.9525"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O224">
        <dia:attribute name="obj_pos">
          <dia:point val="11.1481,9.48814"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.1481,9.24925;11.6935,9.54925"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.1481,9.48814"/>
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
      <dia:object type="Standard - Text" version="0" id="O225">
        <dia:attribute name="obj_pos">
          <dia:point val="11.7981,9.47752"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.7981,9.19882;12.2223,9.54882"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 2#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.437500"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.7981,9.47752"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O226">
        <dia:attribute name="obj_pos">
          <dia:point val="11.0981,8.01314"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.0981,7.77425;11.6435,8.07425"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.0981,8.01314"/>
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
      <dia:object type="Standard - Text" version="0" id="O227">
        <dia:attribute name="obj_pos">
          <dia:point val="11.7481,8.00252"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.7481,7.72382;12.1723,8.07382"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 2#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.437500"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.7481,8.00252"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O228">
        <dia:attribute name="obj_pos">
          <dia:point val="11.0981,6.58814"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.0981,6.34925;11.6435,6.64925"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.0981,6.58814"/>
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
      <dia:object type="Standard - Text" version="0" id="O229">
        <dia:attribute name="obj_pos">
          <dia:point val="11.7481,6.57752"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.7481,6.29882;12.1723,6.64882"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 2#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.437500"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.7481,6.57752"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O230">
        <dia:attribute name="obj_pos">
          <dia:point val="11.1231,5.18814"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.1231,4.94925;11.6685,5.24925"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.1231,5.18814"/>
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
      <dia:object type="Standard - Text" version="0" id="O231">
        <dia:attribute name="obj_pos">
          <dia:point val="11.7731,5.17752"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.7731,4.89882;12.1973,5.24882"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 2#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.437500"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.7731,5.17752"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O232">
        <dia:attribute name="obj_pos">
          <dia:point val="11.1231,16.7214"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.1231,16.4825;11.6685,16.7825"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.1231,16.7214"/>
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
      <dia:object type="Standard - Text" version="0" id="O233">
        <dia:attribute name="obj_pos">
          <dia:point val="11.7731,16.7108"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.7731,16.4321;12.1973,16.7821"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 2#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.437500"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.7731,16.7108"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O234">
        <dia:attribute name="obj_pos">
          <dia:point val="11.1231,15.2214"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.1231,14.9825;11.6685,15.2825"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.1231,15.2214"/>
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
      <dia:object type="Standard - Text" version="0" id="O235">
        <dia:attribute name="obj_pos">
          <dia:point val="11.7731,15.2108"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.7731,14.9321;12.1973,15.2821"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 2#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.437500"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.7731,15.2108"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O236">
        <dia:attribute name="obj_pos">
          <dia:point val="11.1481,13.8464"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.1481,13.6075;11.6935,13.9075"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.1481,13.8464"/>
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
      <dia:object type="Standard - Text" version="0" id="O237">
        <dia:attribute name="obj_pos">
          <dia:point val="11.7981,13.8358"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.7981,13.5571;12.2223,13.9071"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 2#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.437500"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.7981,13.8358"/>
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
    </dia:group>
    <dia:group>
      <dia:object type="Standard - Text" version="0" id="O238">
        <dia:attribute name="obj_pos">
          <dia:point val="11.1231,12.3464"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.1231,12.1075;11.6685,12.4075"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 41#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.375000"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.1231,12.3464"/>
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
      <dia:object type="Standard - Text" version="0" id="O239">
        <dia:attribute name="obj_pos">
          <dia:point val="11.7731,12.3358"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="11.7731,12.0571;12.1973,12.4071"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string># 2#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font name="Courier" family="Takao明朝" style="0"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.437500"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="11.7731,12.3358"/>
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
    </dia:group>
    <dia:object type="Standard - Text" version="0" id="O240">
      <dia:attribute name="obj_pos">
        <dia:point val="13.6731,1.9198"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="13.6731,1.68091;14.9457,2.28091"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#医療機関
コ ー ド#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.6731,1.9198"/>
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
    <dia:object type="Standard - Text" version="0" id="O241">
      <dia:attribute name="obj_pos">
        <dia:point val="2.7875,18.0594"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.7875,17.7807;10.211,18.1307"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※後期高齢者医療は「再掲しないで」下さい。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.7875,18.0594"/>
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
    <dia:object type="Standard - Text" version="0" id="O242">
      <dia:attribute name="obj_pos">
        <dia:point val="2.525,30.5383"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="2.525,30.2596;9.9485,30.6096"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#※後期高齢者医療は「記入しないで」下さい。#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="2.525,30.5383"/>
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
  </dia:layer>
  <dia:layer name="差し込み" visible="true">
    <dia:object type="Embed - Text" version="0" id="O243">
      <dia:attribute name="obj_pos">
        <dia:point val="15.5731,2.1448"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="15.5731,1.74665;17.6941,2.24665"/>
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
            <dia:string>#1234567#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.625000"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="15.5731,2.1448"/>
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
        <dia:int val="7"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O244">
      <dia:attribute name="obj_pos">
        <dia:point val="7.275,3.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,3.4213;7.275,3.7713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[0].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,3.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[0].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O245">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,3.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.7395,3.4213;13.8,3.7713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[0].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,3.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[0].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O246">
      <dia:attribute name="obj_pos">
        <dia:point val="16.9,3.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.9911,3.4213;16.9,3.7713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[0].SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.9,3.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[0].SKINGK#</dia:string>
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
        <dia:point val="19.95,3.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.0411,3.4213;19.95,3.7713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[0].SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,3.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[0].SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O248">
      <dia:attribute name="obj_pos">
        <dia:point val="10.45,3.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,3.4213;10.45,3.7713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[0].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,3.7"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[0].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O249">
      <dia:attribute name="obj_pos">
        <dia:point val="7.275,4.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,4.1713;7.275,4.5213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[0].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,4.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[0].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O250">
      <dia:attribute name="obj_pos">
        <dia:point val="10.45,4.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,4.1713;10.45,4.5213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[0].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,4.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[0].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O251">
      <dia:attribute name="obj_pos">
        <dia:point val="7.275,5.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,4.8713;7.275,5.2213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[1].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,5.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[1].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O252">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,5.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.7395,4.8713;13.8,5.2213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[1].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,5.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[1].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O253">
      <dia:attribute name="obj_pos">
        <dia:point val="16.9,5.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.9911,4.8713;16.9,5.2213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[1].SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.9,5.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[1].SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O254">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,5.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.0411,4.8713;19.95,5.2213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[1].SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,5.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[1].SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O255">
      <dia:attribute name="obj_pos">
        <dia:point val="10.45,5.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,4.8713;10.45,5.2213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[1].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,5.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[1].NYUGAI-TBL[0].ITBFTN#</dia:string>
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
        <dia:point val="7.275,5.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,5.6213;7.275,5.9713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[1].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,5.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[1].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O257">
      <dia:attribute name="obj_pos">
        <dia:point val="10.45,5.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,5.6213;10.45,5.9713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[1].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,5.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[1].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O258">
      <dia:attribute name="obj_pos">
        <dia:point val="7.275,6.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,6.2713;7.275,6.6213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[2].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,6.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[2].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O259">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,6.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.7395,6.2713;13.8,6.6213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[2].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,6.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[2].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O260">
      <dia:attribute name="obj_pos">
        <dia:point val="16.9,6.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.9911,6.2713;16.9,6.6213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[2].SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.9,6.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[2].SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O261">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,6.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.0411,6.2713;19.95,6.6213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[2].SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,6.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[2].SFTNGK#</dia:string>
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
        <dia:point val="10.45,6.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,6.2713;10.45,6.6213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[2].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,6.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[2].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O263">
      <dia:attribute name="obj_pos">
        <dia:point val="7.275,7.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,7.0213;7.275,7.3713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[2].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,7.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[2].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O264">
      <dia:attribute name="obj_pos">
        <dia:point val="10.45,7.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,7.0213;10.45,7.3713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[2].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,7.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[2].NYUGAI-TBL[1].ITBFTN#</dia:string>
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
        <dia:point val="7.275,8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,7.7213;7.275,8.0713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[3].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[3].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O266">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.7395,7.7213;13.8,8.0713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[3].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[3].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O267">
      <dia:attribute name="obj_pos">
        <dia:point val="16.9,8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.9911,7.7213;16.9,8.0713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[3].SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.9,8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[3].SKINGK#</dia:string>
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
        <dia:point val="19.95,8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.0411,7.7213;19.95,8.0713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[3].SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[3].SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O269">
      <dia:attribute name="obj_pos">
        <dia:point val="10.45,8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,7.7213;10.45,8.0713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[3].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[3].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O270">
      <dia:attribute name="obj_pos">
        <dia:point val="7.275,8.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,8.4713;7.275,8.8213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[3].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,8.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[3].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O271">
      <dia:attribute name="obj_pos">
        <dia:point val="10.45,8.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,8.4713;10.45,8.8213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[3].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,8.75"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[3].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O272">
      <dia:attribute name="obj_pos">
        <dia:point val="7.275,9.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,9.1713;7.275,9.5213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[4].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,9.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[4].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O273">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,9.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.7395,9.1713;13.8,9.5213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[4].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,9.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[4].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O274">
      <dia:attribute name="obj_pos">
        <dia:point val="16.9,9.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.9911,9.1713;16.9,9.5213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[4].SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.9,9.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[4].SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O275">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,9.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.0411,9.1713;19.95,9.5213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[4].SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,9.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[4].SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O276">
      <dia:attribute name="obj_pos">
        <dia:point val="10.45,9.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,9.1713;10.45,9.5213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[4].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,9.45"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[4].NYUGAI-TBL[0].ITBFTN#</dia:string>
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
        <dia:point val="7.275,10.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,9.9213;7.275,10.2713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[4].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,10.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[4].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O278">
      <dia:attribute name="obj_pos">
        <dia:point val="10.45,10.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,9.9213;10.45,10.2713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[4].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,10.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[4].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O279">
      <dia:attribute name="obj_pos">
        <dia:point val="7.275,10.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,10.6213;7.275,10.9713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[5].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,10.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[5].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O280">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,10.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.7395,10.6213;13.8,10.9713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[5].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,10.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[5].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O281">
      <dia:attribute name="obj_pos">
        <dia:point val="16.9,10.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.9911,10.6213;16.9,10.9713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[5].SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.9,10.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[5].SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O282">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,10.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.0411,10.6213;19.95,10.9713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[5].SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,10.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[5].SFTNGK#</dia:string>
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
        <dia:point val="10.45,10.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,10.6213;10.45,10.9713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[5].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,10.9"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[5].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O284">
      <dia:attribute name="obj_pos">
        <dia:point val="7.275,11.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,11.3713;7.275,11.7213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[5].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,11.65"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[5].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O285">
      <dia:attribute name="obj_pos">
        <dia:point val="10.45,11.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,11.3713;10.45,11.7213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[5].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,11.65"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[5].NYUGAI-TBL[1].ITBFTN#</dia:string>
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
        <dia:point val="7.275,12.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,12.0213;7.275,12.3713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[6].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,12.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[6].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O287">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,12.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.7395,12.0213;13.8,12.3713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[6].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,12.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[6].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O288">
      <dia:attribute name="obj_pos">
        <dia:point val="16.9,12.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.9911,12.0213;16.9,12.3713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[6].SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.9,12.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[6].SKINGK#</dia:string>
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
        <dia:point val="19.95,12.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.0411,12.0213;19.95,12.3713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[6].SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,12.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[6].SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O290">
      <dia:attribute name="obj_pos">
        <dia:point val="10.45,12.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,12.0213;10.45,12.3713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[6].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,12.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[6].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O291">
      <dia:attribute name="obj_pos">
        <dia:point val="7.275,13.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,12.7713;7.275,13.1213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[6].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,13.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[6].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O292">
      <dia:attribute name="obj_pos">
        <dia:point val="10.45,13.05"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,12.7713;10.45,13.1213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[6].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,13.05"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[6].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O293">
      <dia:attribute name="obj_pos">
        <dia:point val="7.275,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,13.5213;7.275,13.8713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[7].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,13.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[7].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O294">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.7395,13.5213;13.8,13.8713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[7].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,13.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[7].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O295">
      <dia:attribute name="obj_pos">
        <dia:point val="16.9,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.9911,13.5213;16.9,13.8713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[7].SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.9,13.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[7].SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O296">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.0411,13.5213;19.95,13.8713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[7].SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,13.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[7].SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O297">
      <dia:attribute name="obj_pos">
        <dia:point val="10.45,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,13.5213;10.45,13.8713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[7].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,13.8"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[7].NYUGAI-TBL[0].ITBFTN#</dia:string>
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
        <dia:point val="7.275,14.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,14.2713;7.275,14.6213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[7].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,14.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[7].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O299">
      <dia:attribute name="obj_pos">
        <dia:point val="10.45,14.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,14.2713;10.45,14.6213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[7].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,14.55"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[7].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O300">
      <dia:attribute name="obj_pos">
        <dia:point val="7.275,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,14.9213;7.275,15.2713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[8].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,15.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[8].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O301">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.7395,14.9213;13.8,15.2713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[8].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,15.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[8].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O302">
      <dia:attribute name="obj_pos">
        <dia:point val="16.9,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.9911,14.9213;16.9,15.2713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[8].SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.9,15.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[8].SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O303">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.0411,14.9213;19.95,15.2713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[8].SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,15.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[8].SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O304">
      <dia:attribute name="obj_pos">
        <dia:point val="10.45,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,14.9213;10.45,15.2713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[8].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,15.2"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[8].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O305">
      <dia:attribute name="obj_pos">
        <dia:point val="7.275,15.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,15.6713;7.275,16.0213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[8].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,15.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[8].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O306">
      <dia:attribute name="obj_pos">
        <dia:point val="10.45,15.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,15.6713;10.45,16.0213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[8].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,15.95"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[8].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O307">
      <dia:attribute name="obj_pos">
        <dia:point val="7.275,16.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,16.3713;7.275,16.7213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[9].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,16.65"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[9].NYUGAI-TBL[0].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O308">
      <dia:attribute name="obj_pos">
        <dia:point val="13.8,16.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.7395,16.3713;13.8,16.7213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[9].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="13.8,16.65"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[9].SKENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O309">
      <dia:attribute name="obj_pos">
        <dia:point val="16.9,16.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="14.9911,16.3713;16.9,16.7213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[9].SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="16.9,16.65"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[9].SKINGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O310">
      <dia:attribute name="obj_pos">
        <dia:point val="19.95,16.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="18.0411,16.3713;19.95,16.7213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#SHOKUJI[9].SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="19.95,16.65"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#SHOKUJI[9].SFTNGK#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O311">
      <dia:attribute name="obj_pos">
        <dia:point val="10.45,16.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,16.3713;10.45,16.7213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[9].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,16.65"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[9].NYUGAI-TBL[0].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O312">
      <dia:attribute name="obj_pos">
        <dia:point val="7.275,17.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.2145,17.1213;7.275,17.4713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[9].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.275,17.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[9].NYUGAI-TBL[1].KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O313">
      <dia:attribute name="obj_pos">
        <dia:point val="10.45,17.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="8.5411,17.1213;10.45,17.4713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[9].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="10.45,17.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#KOHI[9].NYUGAI-TBL[1].ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O314">
      <dia:attribute name="obj_pos">
        <dia:point val="7.65,20.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.5895,20.3213;7.65,20.6713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[0].Z-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.65,20.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[0].Z-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O315">
      <dia:attribute name="obj_pos">
        <dia:point val="11.5,20.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.379,20.3213;11.5,20.6713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[0].Z-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.5,20.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[0].Z-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O316">
      <dia:attribute name="obj_pos">
        <dia:point val="14.75,20.6"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.8411,20.3213;14.75,20.6713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[0].Z-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.75,20.6"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[0].Z-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O317">
      <dia:attribute name="obj_pos">
        <dia:point val="7.66582,29.6733"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.60532,29.3946;7.66582,29.7446"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU-KOHI[2].ZK-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66582,29.6733"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU-KOHI[2].ZK-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O318">
      <dia:attribute name="obj_pos">
        <dia:point val="7.66582,28.9733"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.60532,28.6946;7.66582,29.0446"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU-KOHI[1].ZK-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.66582,28.9733"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU-KOHI[1].ZK-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O319">
      <dia:attribute name="obj_pos">
        <dia:point val="7.67532,28.2272"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.61482,27.9485;7.67532,28.2985"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU-KOHI[0].ZK-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.67532,28.2272"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU-KOHI[0].ZK-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O320">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4937,29.6279"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.5848,29.3492;11.4937,29.6992"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU-KOHI[2].ZK-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4937,29.6279"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU-KOHI[2].ZK-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O321">
      <dia:attribute name="obj_pos">
        <dia:point val="11.4937,28.9529"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.5848,28.6742;11.4937,29.0242"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU-KOHI[1].ZK-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.4937,28.9529"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU-KOHI[1].ZK-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O322">
      <dia:attribute name="obj_pos">
        <dia:point val="11.5003,28.2022"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.5914,27.9235;11.5003,28.2735"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU-KOHI[0].ZK-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.5003,28.2022"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU-KOHI[0].ZK-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O323">
      <dia:attribute name="obj_pos">
        <dia:point val="7.65,21.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.5895,21.1213;7.65,21.4713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[1].Z-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.65,21.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[1].Z-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O324">
      <dia:attribute name="obj_pos">
        <dia:point val="11.5,21.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.379,21.1213;11.5,21.4713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[1].Z-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.5,21.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[1].Z-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O325">
      <dia:attribute name="obj_pos">
        <dia:point val="14.75,21.4"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.8411,21.1213;14.75,21.4713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[1].Z-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.75,21.4"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[1].Z-ITBFTN#</dia:string>
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
        <dia:point val="7.65,22.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.5895,22.0213;7.65,22.3713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[2].Z-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.65,22.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[2].Z-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O327">
      <dia:attribute name="obj_pos">
        <dia:point val="11.5,22.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.379,22.0213;11.5,22.3713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[2].Z-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.5,22.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[2].Z-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O328">
      <dia:attribute name="obj_pos">
        <dia:point val="14.75,22.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.8411,22.0213;14.75,22.3713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[2].Z-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.75,22.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[2].Z-ITBFTN#</dia:string>
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
        <dia:point val="7.65,23.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.5895,23.0213;7.65,23.3713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[3].Z-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.65,23.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[3].Z-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O330">
      <dia:attribute name="obj_pos">
        <dia:point val="11.5,23.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.379,23.0213;11.5,23.3713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[3].Z-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.5,23.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[3].Z-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O331">
      <dia:attribute name="obj_pos">
        <dia:point val="14.75,23.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.8411,23.0213;14.75,23.3713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[3].Z-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.75,23.3"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[3].Z-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O332">
      <dia:attribute name="obj_pos">
        <dia:point val="7.65,24.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.5895,23.9713;7.65,24.3213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[4].Z-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.65,24.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[4].Z-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O333">
      <dia:attribute name="obj_pos">
        <dia:point val="11.5,24.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.379,23.9713;11.5,24.3213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[4].Z-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.5,24.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[4].Z-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O334">
      <dia:attribute name="obj_pos">
        <dia:point val="14.75,24.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.8411,23.9713;14.75,24.3213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[4].Z-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.75,24.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[4].Z-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="9"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O335">
      <dia:attribute name="obj_pos">
        <dia:point val="7.65,25.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.5895,24.9713;7.65,25.3213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[5].Z-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.65,25.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[5].Z-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O336">
      <dia:attribute name="obj_pos">
        <dia:point val="11.5,25.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.379,24.9713;11.5,25.3213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[5].Z-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.5,25.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[5].Z-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O337">
      <dia:attribute name="obj_pos">
        <dia:point val="14.75,25.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.8411,24.9713;14.75,25.3213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[5].Z-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.75,25.25"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[5].Z-ITBFTN#</dia:string>
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
        <dia:point val="7.65,26.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="6.5895,25.8713;7.65,26.2213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[6].Z-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="7.65,26.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[6].Z-KENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="5"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O339">
      <dia:attribute name="obj_pos">
        <dia:point val="11.5,26.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.379,25.8713;11.5,26.2213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[6].Z-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="11.5,26.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[6].Z-TENSU#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O340">
      <dia:attribute name="obj_pos">
        <dia:point val="14.75,26.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="12.8411,25.8713;14.75,26.2213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU[6].Z-ITBFTN#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="1"/>
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
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="14.75,26.15"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="2"/>
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
        <dia:string>#ZAITAKU[6].Z-ITBFTN#</dia:string>
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
        <dia:point val="1.8,3.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.8,3.4213;2.2242,3.7713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[0].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.8,3.7"/>
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
        <dia:string>#KOHI[0].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O342">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,5.15"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.8,4.8713;2.2242,5.2213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[1].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.8,5.15"/>
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
        <dia:string>#KOHI[1].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O343">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,6.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.8,6.2713;2.2242,6.6213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[2].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.8,6.55"/>
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
        <dia:string>#KOHI[2].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O344">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.8,7.7213;2.2242,8.0713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[3].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.8,8"/>
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
        <dia:string>#KOHI[3].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O345">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,9.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.8,9.1713;2.2242,9.5213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[4].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.8,9.45"/>
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
        <dia:string>#KOHI[4].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O346">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,10.9"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.8,10.6213;2.2242,10.9713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[5].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.8,10.9"/>
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
        <dia:string>#KOHI[5].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O347">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,12.3"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.8,12.0213;2.2242,12.3713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[6].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.8,12.3"/>
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
        <dia:string>#KOHI[6].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O348">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,13.8"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.8,13.5213;2.2242,13.8713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[7].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.8,13.8"/>
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
        <dia:string>#KOHI[7].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O349">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,15.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.8,14.9213;2.2242,15.2713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[8].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.8,15.2"/>
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
        <dia:string>#KOHI[8].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O350">
      <dia:attribute name="obj_pos">
        <dia:point val="1.8,16.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.8,16.3713;2.2242,16.7213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[9].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.8,16.65"/>
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
        <dia:string>#KOHI[9].HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O351">
      <dia:attribute name="obj_pos">
        <dia:point val="1.25,4.25"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.25,3.9713;3.0175,4.3213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[0].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.25,4.25"/>
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
        <dia:string>#KOHI[0].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O352">
      <dia:attribute name="obj_pos">
        <dia:point val="1.25,5.7"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.25,5.4213;3.0175,5.7713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[1].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.25,5.7"/>
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
        <dia:string>#KOHI[1].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O353">
      <dia:attribute name="obj_pos">
        <dia:point val="1.25,7.1"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.25,6.8213;3.0175,7.1713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[2].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.25,7.1"/>
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
        <dia:string>#KOHI[2].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O354">
      <dia:attribute name="obj_pos">
        <dia:point val="1.25,8.55"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.25,8.2713;3.0175,8.6213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[3].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.25,8.55"/>
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
        <dia:string>#KOHI[3].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O355">
      <dia:attribute name="obj_pos">
        <dia:point val="1.25,10"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.25,9.7213;3.0175,10.0713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[4].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.25,10"/>
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
        <dia:string>#KOHI[4].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O356">
      <dia:attribute name="obj_pos">
        <dia:point val="1.25,11.45"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.25,11.1713;3.0175,11.5213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[5].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.25,11.45"/>
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
        <dia:string>#KOHI[5].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O357">
      <dia:attribute name="obj_pos">
        <dia:point val="1.25,12.85"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.25,12.5713;3.0175,12.9213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[6].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.25,12.85"/>
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
        <dia:string>#KOHI[6].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O358">
      <dia:attribute name="obj_pos">
        <dia:point val="1.25,14.35"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.25,14.0713;3.0175,14.4213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[7].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.25,14.35"/>
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
        <dia:string>#KOHI[7].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O359">
      <dia:attribute name="obj_pos">
        <dia:point val="1.25,15.75"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.25,15.4713;3.0175,15.8213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[8].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.25,15.75"/>
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
        <dia:string>#KOHI[8].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O360">
      <dia:attribute name="obj_pos">
        <dia:point val="1.25,17.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.25,16.9213;3.0175,17.2713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#KOHI[9].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.25,17.2"/>
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
        <dia:string>#KOHI[9].KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O361">
      <dia:attribute name="obj_pos">
        <dia:point val="1.9,29.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.9,29.3713;3.6675,29.7213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU-KOHI[2].Z-KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.9,29.65"/>
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
        <dia:string>#ZAITAKU-KOHI[2].Z-KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O362">
      <dia:attribute name="obj_pos">
        <dia:point val="1.9,28.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.9,28.6713;3.6675,29.0213"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU-KOHI[1].Z-KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.9,28.95"/>
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
        <dia:string>#ZAITAKU-KOHI[1].Z-KOHNAME#</dia:string>
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
        <dia:point val="1.9,28.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.9,27.9213;3.6675,28.2713"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU-KOHI[0].Z-KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.437500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.9,28.2"/>
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
        <dia:string>#ZAITAKU-KOHI[0].Z-KOHNAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="10"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O364">
      <dia:attribute name="obj_pos">
        <dia:point val="1.15,28.2"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.15,27.8417;1.6954,28.2917"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU-KOHI[0].Z-HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.15,28.2"/>
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
        <dia:string>#ZAITAKU-KOHI[0].Z-HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O365">
      <dia:attribute name="obj_pos">
        <dia:point val="1.15,28.95"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.15,28.5917;1.6954,29.0417"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU-KOHI[1].Z-HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.15,28.95"/>
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
        <dia:string>#ZAITAKU-KOHI[1].Z-HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O366">
      <dia:attribute name="obj_pos">
        <dia:point val="1.15,29.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.15,29.2917;1.6954,29.7417"/>
      </dia:attribute>
      <dia:attribute name="dnode_path">
        <dia:string>#ZAITAKU-KOHI[2].Z-HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_string">
        <dia:enum val="0"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#99#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font name="Courier" family="Takao明朝" style="0"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.562500"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.15,29.65"/>
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
        <dia:string>#ZAITAKU-KOHI[2].Z-HBTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_text_size">
        <dia:int val="2"/>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="0" id="O367">
      <dia:attribute name="obj_pos">
        <dia:point val="9.13972,1.84397"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="9.13972,1.52545;11.1597,1.92545"/>
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
            <dia:point val="9.13972,1.84397"/>
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
