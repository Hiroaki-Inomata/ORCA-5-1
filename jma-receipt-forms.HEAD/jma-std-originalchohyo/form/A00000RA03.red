<?xml version="1.0" encoding="UTF-8"?>
<dia:diagram xmlns:dia="http://www.lysator.liu.se/~alla/dia/">
  <dia:dictionarydata>
    <element name="PTNUM" occurs="1">
      <appinfo>
        <embed object="text" length="23"/>
      </appinfo>
    </element>
    <element name="NAME" occurs="1">
      <appinfo>
        <embed object="text" length="56"/>
      </appinfo>
    </element>
    <element name="NAME2" occurs="1">
      <appinfo>
        <embed object="text" length="64"/>
      </appinfo>
    </element>
    <element name="HUKUYO" occurs="1">
      <appinfo>
        <embed object="text" length="4"/>
      </appinfo>
    </element>
    <element name="NAIYO-G" occurs="22">
      <element name="NAIYO" occurs="1">
        <appinfo>
          <embed object="text" length="40"/>
        </appinfo>
      </element>
      <element name="NAIYO2" occurs="1">
        <appinfo>
          <embed object="text" length="40"/>
        </appinfo>
      </element>
    </element>
    <element name="SRYYMD" occurs="1">
      <appinfo>
        <embed object="text" length="22"/>
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
    <dia:object type="Standard - Line" version="0" id="O0">
      <dia:attribute name="obj_pos">
        <dia:point val="0.4,2.525"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.4,2.525;7.5,2.525"/>
      </dia:attribute>
      <dia:attribute name="conn_endpoints">
        <dia:point val="0.4,2.525"/>
        <dia:point val="7.5,2.525"/>
      </dia:attribute>
      <dia:attribute name="numcp">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="line_width">
        <dia:real val="0"/>
      </dia:attribute>
    </dia:object>
  </dia:layer>
  <dia:layer name="文字" visible="true">
    <dia:object type="Standard - Text" version="1" id="O1">
      <dia:attribute name="obj_pos">
        <dia:point val="1.775,3.475"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.775,3.1225;6.175,3.525"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#＊＊＊＊ 用法 ＊＊＊＊#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takaoゴシック" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.775,3.475"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
          <dia:attribute name="alignment">
            <dia:enum val="0"/>
          </dia:attribute>
        </dia:composite>
      </dia:attribute>
      <dia:attribute name="valign">
        <dia:enum val="3"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Standard - Text" version="1" id="O2">
      <dia:attribute name="obj_pos">
        <dia:point val="0.4,1.65"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.4,1.385;1.3,1.6875"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ＮＯ．#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takaoゴシック" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="0.4,1.65"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
          </dia:attribute>
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
  <dia:layer name="差込２" visible="false"/>
  <dia:layer name="差込" visible="true" active="true">
    <dia:object type="Embed - Text" version="1" id="O3">
      <dia:attribute name="obj_pos">
        <dia:point val="0.325,2.075"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.325,1.6775;7.525,2.6925"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#いいいいいいいいいいいいいいいい
いいいいいいいいいいいいいいいい#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takaoゴシック" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.5625"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="0.325,2.075"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#NAME2#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="15"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O4">
      <dia:attribute name="obj_pos">
        <dia:point val="0.3,2.375"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.3,1.8275;6.5,2.45"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#ああああ ああああ 樣#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takaoゴシック" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.77611108124256134"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="0.3,2.375"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#NAME#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:group>
      <dia:object type="Embed - Text" version="1" id="O5">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,3.975"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,3.65;7.25,4.02"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,3.975"/>
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
          <dia:string>#NAIYO-G[0].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O6">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,4.4369"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,4.1119;7.25,4.4819"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,4.4369"/>
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
          <dia:string>#NAIYO-G[1].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O7">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,4.89881"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,4.57381;7.25,4.94381"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,4.89881"/>
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
          <dia:string>#NAIYO-G[2].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O8">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,5.36071"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,5.03571;7.25,5.40571"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,5.36071"/>
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
          <dia:string>#NAIYO-G[3].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O9">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,6.07262"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,5.74762;7.25,6.11762"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,6.07262"/>
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
          <dia:string>#NAIYO-G[4].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O10">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,6.53452"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,6.20952;7.25,6.57952"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,6.53452"/>
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
          <dia:string>#NAIYO-G[5].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O11">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,6.99643"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,6.67143;7.25,7.04143"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,6.99643"/>
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
          <dia:string>#NAIYO-G[6].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O12">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,7.45833"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,7.13333;7.25,7.50333"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,7.45833"/>
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
          <dia:string>#NAIYO-G[7].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O13">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,7.92024"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,7.59524;7.25,7.96524"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,7.92024"/>
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
          <dia:string>#NAIYO-G[8].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O14">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,8.38214"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,8.05714;7.25,8.42714"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,8.38214"/>
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
          <dia:string>#NAIYO-G[9].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O15">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,9.09405"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,8.76905;7.25,9.13905"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,9.09405"/>
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
          <dia:string>#NAIYO-G[10].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O16">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,9.55595"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,9.23095;7.25,9.60095"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,9.55595"/>
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
          <dia:string>#NAIYO-G[11].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O17">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,10.0179"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,9.6929;7.25,10.0629"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,10.0179"/>
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
          <dia:string>#NAIYO-G[12].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O18">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,10.4798"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,10.1548;7.25,10.5248"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,10.4798"/>
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
          <dia:string>#NAIYO-G[13].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O19">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,10.9417"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,10.6167;7.25,10.9867"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,10.9417"/>
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
          <dia:string>#NAIYO-G[14].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O20">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,11.4036"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,11.0786;7.25,11.4486"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,11.4036"/>
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
          <dia:string>#NAIYO-G[15].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O21">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,12.1155"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,11.7905;7.25,12.1605"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,12.1155"/>
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
          <dia:string>#NAIYO-G[16].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O22">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,12.5524"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,12.2274;7.25,12.5974"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,12.5524"/>
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
          <dia:string>#NAIYO-G[17].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O23">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,12.9893"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,12.6643;7.25,13.0343"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,12.9893"/>
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
          <dia:string>#NAIYO-G[18].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O24">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,13.4262"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,13.1012;7.25,13.4712"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,13.4262"/>
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
          <dia:string>#NAIYO-G[19].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O25">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,13.8631"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,13.5381;7.25,13.9081"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,13.8631"/>
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
          <dia:string>#NAIYO-G[20].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O26">
        <dia:attribute name="obj_pos">
          <dia:point val="7.25,14.3"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.635,13.975;7.25,14.345"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="7.25,14.3"/>
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
          <dia:string>#NAIYO-G[21].NAIYO2#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
    </dia:group>
    <dia:object type="Embed - Text" version="1" id="O27">
      <dia:attribute name="obj_pos">
        <dia:point val="0.4,1.275"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="0.4,0.9825;4.03,1.315"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#あああああああああああ#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takaoゴシック" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.41249999999999998"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="0.4,1.275"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#SRYYMD#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O28">
      <dia:attribute name="obj_pos">
        <dia:point val="3.125,0.70741"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="3.125,0.05991;4.595,0.79741"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#内服#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takaoゴシック" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.91722218692302704"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="3.125,0.70741"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#HUKUYO#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:object type="Embed - Text" version="1" id="O29">
      <dia:attribute name="obj_pos">
        <dia:point val="1.075,1.625"/>
      </dia:attribute>
      <dia:attribute name="obj_bb">
        <dia:rectangle val="1.075,1.36;2.875,1.6625"/>
      </dia:attribute>
      <dia:attribute name="text">
        <dia:composite type="text">
          <dia:attribute name="string">
            <dia:string>#０００００１#</dia:string>
          </dia:attribute>
          <dia:attribute name="font">
            <dia:font family="Takaoゴシック" style="0" name="Courier"/>
          </dia:attribute>
          <dia:attribute name="height">
            <dia:real val="0.375"/>
          </dia:attribute>
          <dia:attribute name="pos">
            <dia:point val="1.075,1.625"/>
          </dia:attribute>
          <dia:attribute name="color">
            <dia:color val="#000000"/>
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
        <dia:string>#PTNUM#</dia:string>
      </dia:attribute>
      <dia:attribute name="embed_column_size">
        <dia:int val="0"/>
      </dia:attribute>
      <dia:attribute name="embed_char_type">
        <dia:enum val="0"/>
      </dia:attribute>
    </dia:object>
    <dia:group>
      <dia:object type="Embed - Text" version="1" id="O30">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,3.975"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,3.65;7.0275,4.02"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,3.975"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[0].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O31">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,4.4369"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,4.1119;7.0275,4.4819"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,4.4369"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[1].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O32">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,4.89881"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,4.57381;7.0275,4.94381"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,4.89881"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[2].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O33">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,5.36071"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,5.03571;7.0275,5.40571"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,5.36071"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[3].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O34">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,6.07262"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,5.74762;7.0275,6.11762"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,6.07262"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[4].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O35">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,6.53452"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,6.20952;7.0275,6.57952"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,6.53452"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[5].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O36">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,6.99643"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,6.67143;7.0275,7.04143"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,6.99643"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[6].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O37">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,7.45833"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,7.13333;7.0275,7.50333"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,7.45833"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[7].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O38">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,7.92024"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,7.59524;7.0275,7.96524"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,7.92024"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[8].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O39">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,8.38214"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,8.05714;7.0275,8.42714"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,8.38214"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[9].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O40">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,9.09405"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,8.76905;7.0275,9.13905"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,9.09405"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[10].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O41">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,9.55595"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,9.23095;7.0275,9.60095"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,9.55595"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[11].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O42">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,10.0179"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,9.6929;7.0275,10.0629"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,10.0179"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[12].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O43">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,10.4798"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,10.1548;7.0275,10.5248"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,10.4798"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[13].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O44">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,10.9417"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,10.6167;7.0275,10.9867"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,10.9417"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[14].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O45">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,11.4036"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,11.0786;7.0275,11.4486"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,11.4036"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[15].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O46">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,12.1155"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,11.7905;7.0275,12.1605"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,12.1155"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[16].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O47">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,12.5524"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,12.2274;7.0275,12.5974"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,12.5524"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[17].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O48">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,12.9893"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,12.6643;7.0275,13.0343"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,12.9893"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[18].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O49">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,13.4262"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,13.1012;7.0275,13.4712"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,13.4262"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[19].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O50">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,13.8631"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,13.5381;7.0275,13.9081"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,13.8631"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[20].NAIYO#</dia:string>
        </dia:attribute>
        <dia:attribute name="embed_column_size">
          <dia:int val="0"/>
        </dia:attribute>
        <dia:attribute name="embed_char_type">
          <dia:enum val="0"/>
        </dia:attribute>
      </dia:object>
      <dia:object type="Embed - Text" version="1" id="O51">
        <dia:attribute name="obj_pos">
          <dia:point val="0.4125,14.3"/>
        </dia:attribute>
        <dia:attribute name="obj_bb">
          <dia:rectangle val="0.4125,13.975;7.0275,14.345"/>
        </dia:attribute>
        <dia:attribute name="text">
          <dia:composite type="text">
            <dia:attribute name="string">
              <dia:string>#ああああああああああああああああああ#</dia:string>
            </dia:attribute>
            <dia:attribute name="font">
              <dia:font family="Takaoゴシック" style="0" name="Courier"/>
            </dia:attribute>
            <dia:attribute name="height">
              <dia:real val="0.45861109346151352"/>
            </dia:attribute>
            <dia:attribute name="pos">
              <dia:point val="0.4125,14.3"/>
            </dia:attribute>
            <dia:attribute name="color">
              <dia:color val="#000000"/>
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
          <dia:string>#NAIYO-G[21].NAIYO#</dia:string>
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
