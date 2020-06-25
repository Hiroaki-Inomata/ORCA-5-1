<?xml version="1.0"?>
<diagram xmlns:dia="http://www.lysator.liu.se/~alla/dia/">
  <dictionarydata>
    <element name="SRYYM" occurs="1">
      <appinfo>
        <embed object="string" length="16"/>
      </appinfo>
    </element>
    <element name="SEIYMD" occurs="1">
      <appinfo>
        <embed object="string" length="22"/>
      </appinfo>
    </element>
    <element name="HOSPCDN" occurs="1">
      <appinfo>
        <embed object="string" length="20"/>
      </appinfo>
    </element>
    <element name="ADRS" occurs="1">
      <appinfo>
        <embed object="string" length="100"/>
      </appinfo>
    </element>
    <element name="HOSPNAME" occurs="1">
      <appinfo>
        <embed object="string" length="100"/>
      </appinfo>
    </element>
    <element name="TEL" occurs="1">
      <appinfo>
        <embed object="string" length="15"/>
      </appinfo>
    </element>
    <element name="KAISETU" occurs="1">
      <appinfo>
        <embed object="string" length="30"/>
      </appinfo>
    </element>
    <element name="TENSUHYO" occurs="4">
      <appinfo>
        <embed object="string" length="2"/>
      </appinfo>
    </element>
    <element name="CITYNAME" occurs="1">
      <appinfo>
        <embed object="string" length="26"/>
      </appinfo>
    </element>
    <element name="SEIKYU-AREA" occurs="1">
      <element name="KENSU" occurs="1">
        <appinfo>
          <embed object="string" length="6"/>
        </appinfo>
      </element>
      <element name="SEIKYUGK" occurs="1">
        <appinfo>
          <embed object="string" length="11"/>
        </appinfo>
      </element>
    </element>
  </dictionarydata>
  <diagramdata>
    <attribute name="background">
      <color val="#ffffff"/>
    </attribute>
    <attribute name="paper">
      <composite type="paper">
        <attribute name="name">
          <string>#A4#</string>
        </attribute>
        <attribute name="pswidth">
          <real val="21"/>
        </attribute>
        <attribute name="psheight">
          <real val="29.7"/>
        </attribute>
        <attribute name="tmargin">
          <real val="0"/>
        </attribute>
        <attribute name="bmargin">
          <real val="0"/>
        </attribute>
        <attribute name="lmargin">
          <real val="0"/>
        </attribute>
        <attribute name="rmargin">
          <real val="0"/>
        </attribute>
        <attribute name="is_portrait">
          <boolean val="true"/>
        </attribute>
        <attribute name="scaling">
          <real val="1"/>
        </attribute>
        <attribute name="fitto">
          <boolean val="false"/>
        </attribute>
      </composite>
    </attribute>
  </diagramdata>
  <layer name="&#199;&#216;&#183;&#202;" visible="true">
    <object type="Standard - Box" version="0" id="O0">
      <attribute name="obj_pos">
        <point val="1.48337,2.6"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="1.46837,2.585;19.4984,27.6007"/>
      </attribute>
      <attribute name="elem_corner">
        <point val="1.48337,2.6"/>
      </attribute>
      <attribute name="elem_width">
        <real val="18"/>
      </attribute>
      <attribute name="elem_height">
        <real val="24.9857"/>
      </attribute>
      <attribute name="border_width">
        <real val="0.03"/>
      </attribute>
      <attribute name="show_background">
        <boolean val="false"/>
      </attribute>
    </object>
    <object type="Standard - Box" version="0" id="O1">
      <attribute name="obj_pos">
        <point val="1.73337,15.2367"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="1.72837,15.2317;19.2884,24.2417"/>
      </attribute>
      <attribute name="elem_corner">
        <point val="1.73337,15.2367"/>
      </attribute>
      <attribute name="elem_width">
        <real val="17.55"/>
      </attribute>
      <attribute name="elem_height">
        <real val="9"/>
      </attribute>
      <attribute name="border_width">
        <real val="0.01"/>
      </attribute>
      <attribute name="show_background">
        <boolean val="false"/>
      </attribute>
    </object>
    <object type="Standard - Line" version="0" id="O2">
      <attribute name="obj_pos">
        <point val="1.73337,20.2318"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="1.71837,20.2168;19.2984,20.2468"/>
      </attribute>
      <attribute name="conn_endpoints">
        <point val="1.73337,20.2318"/>
        <point val="19.2834,20.2318"/>
      </attribute>
      <attribute name="numcp">
        <int val="1"/>
      </attribute>
      <attribute name="line_width">
        <real val="0.03"/>
      </attribute>
    </object>
    <object type="Standard - Line" version="0" id="O3">
      <attribute name="obj_pos">
        <point val="1.73337,15.2367"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="1.71837,15.2217;19.2984,15.2517"/>
      </attribute>
      <attribute name="conn_endpoints">
        <point val="1.73337,15.2367"/>
        <point val="19.2834,15.2367"/>
      </attribute>
      <attribute name="numcp">
        <int val="1"/>
      </attribute>
      <attribute name="line_width">
        <real val="0.03"/>
      </attribute>
      <connections>
        <connection handle="0" to="O1" connection="0"/>
        <connection handle="1" to="O1" connection="2"/>
      </connections>
    </object>
    <object type="Standard - Line" version="0" id="O4">
      <attribute name="obj_pos">
        <point val="1.73337,15.2367"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="1.71053,15.2217;1.74837,20.2405"/>
      </attribute>
      <attribute name="conn_endpoints">
        <point val="1.73337,15.2367"/>
        <point val="1.72553,20.2255"/>
      </attribute>
      <attribute name="numcp">
        <int val="1"/>
      </attribute>
      <attribute name="line_width">
        <real val="0.03"/>
      </attribute>
      <connections>
        <connection handle="0" to="O1" connection="0"/>
      </connections>
    </object>
    <object type="Standard - Line" version="0" id="O5">
      <attribute name="obj_pos">
        <point val="19.2834,15.2367"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="19.2609,15.2217;19.2984,20.2405"/>
      </attribute>
      <attribute name="conn_endpoints">
        <point val="19.2834,15.2367"/>
        <point val="19.2759,20.2255"/>
      </attribute>
      <attribute name="numcp">
        <int val="1"/>
      </attribute>
      <attribute name="line_width">
        <real val="0.03"/>
      </attribute>
      <connections>
        <connection handle="0" to="O1" connection="2"/>
      </connections>
    </object>
    <object type="Standard - Box" version="0" id="O6">
      <attribute name="obj_pos">
        <point val="10.0834,5.18674"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="10.0684,5.17174;19.065,13.2017"/>
      </attribute>
      <attribute name="elem_corner">
        <point val="10.0834,5.18674"/>
      </attribute>
      <attribute name="elem_width">
        <real val="8.9666"/>
      </attribute>
      <attribute name="elem_height">
        <real val="8"/>
      </attribute>
      <attribute name="border_width">
        <real val="0.03"/>
      </attribute>
      <attribute name="show_background">
        <boolean val="false"/>
      </attribute>
    </object>
    <object type="Standard - Line" version="0" id="O7">
      <attribute name="obj_pos">
        <point val="4.23157,4.51649"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="4.22657,4.51149;16.6866,4.52149"/>
      </attribute>
      <attribute name="conn_endpoints">
        <point val="4.23157,4.51649"/>
        <point val="16.6816,4.51649"/>
      </attribute>
      <attribute name="numcp">
        <int val="1"/>
      </attribute>
      <attribute name="line_width">
        <real val="0.01"/>
      </attribute>
    </object>
    <object type="Standard - Line" version="0" id="O8">
      <attribute name="obj_pos">
        <point val="4.23047,4.42803"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="4.22547,4.42303;16.6855,4.43303"/>
      </attribute>
      <attribute name="conn_endpoints">
        <point val="4.23047,4.42803"/>
        <point val="16.6805,4.42803"/>
      </attribute>
      <attribute name="numcp">
        <int val="1"/>
      </attribute>
      <attribute name="line_width">
        <real val="0.01"/>
      </attribute>
    </object>
    <object type="Standard - Line" version="0" id="O9">
      <attribute name="obj_pos">
        <point val="10.1012,5.99518"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="10.0962,5.99018;19.0425,6.005"/>
      </attribute>
      <attribute name="conn_endpoints">
        <point val="10.1012,5.99518"/>
        <point val="19.0375,6"/>
      </attribute>
      <attribute name="numcp">
        <int val="1"/>
      </attribute>
      <attribute name="line_width">
        <real val="0.01"/>
      </attribute>
    </object>
    <object type="Standard - Line" version="0" id="O10">
      <attribute name="obj_pos">
        <point val="10.1009,6.83292"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="10.0959,6.82;19.055,6.83792"/>
      </attribute>
      <attribute name="conn_endpoints">
        <point val="10.1009,6.83292"/>
        <point val="19.05,6.825"/>
      </attribute>
      <attribute name="numcp">
        <int val="1"/>
      </attribute>
      <attribute name="line_width">
        <real val="0.01"/>
      </attribute>
    </object>
    <object type="Standard - Line" version="0" id="O11">
      <attribute name="obj_pos">
        <point val="10.1009,7.68158"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="10.0959,7.67;19.03,7.68658"/>
      </attribute>
      <attribute name="conn_endpoints">
        <point val="10.1009,7.68158"/>
        <point val="19.025,7.675"/>
      </attribute>
      <attribute name="numcp">
        <int val="1"/>
      </attribute>
      <attribute name="line_width">
        <real val="0.01"/>
      </attribute>
    </object>
    <object type="Standard - Line" version="0" id="O12">
      <attribute name="obj_pos">
        <point val="10.1009,8.51255"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="10.0959,8.50755;19.03,8.53"/>
      </attribute>
      <attribute name="conn_endpoints">
        <point val="10.1009,8.51255"/>
        <point val="19.025,8.525"/>
      </attribute>
      <attribute name="numcp">
        <int val="1"/>
      </attribute>
      <attribute name="line_width">
        <real val="0.01"/>
      </attribute>
    </object>
    <object type="Standard - Line" version="0" id="O13">
      <attribute name="obj_pos">
        <point val="13.2498,5.18865"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="13.2448,5.18365;13.2551,13.2019"/>
      </attribute>
      <attribute name="conn_endpoints">
        <point val="13.2498,5.18865"/>
        <point val="13.2501,13.1969"/>
      </attribute>
      <attribute name="numcp">
        <int val="1"/>
      </attribute>
      <attribute name="line_width">
        <real val="0.01"/>
      </attribute>
    </object>
    <object type="Standard - Line" version="0" id="O14">
      <attribute name="obj_pos">
        <point val="1.73356,16.0018"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="1.72856,15.99;19.2912,16.0068"/>
      </attribute>
      <attribute name="conn_endpoints">
        <point val="1.73356,16.0018"/>
        <point val="19.2862,15.995"/>
      </attribute>
      <attribute name="numcp">
        <int val="1"/>
      </attribute>
      <attribute name="line_width">
        <real val="0.01"/>
      </attribute>
    </object>
    <object type="Standard - Line" version="0" id="O15">
      <attribute name="obj_pos">
        <point val="4.0074,15.2416"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="4.0024,15.2366;4.0183,24.2443"/>
      </attribute>
      <attribute name="conn_endpoints">
        <point val="4.0074,15.2416"/>
        <point val="4.0133,24.2393"/>
      </attribute>
      <attribute name="numcp">
        <int val="1"/>
      </attribute>
      <attribute name="line_width">
        <real val="0.01"/>
      </attribute>
    </object>
    <object type="Standard - Line" version="0" id="O16">
      <attribute name="obj_pos">
        <point val="10.1071,15.2416"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="10.1021,15.2366;10.1182,24.2443"/>
      </attribute>
      <attribute name="conn_endpoints">
        <point val="10.1071,15.2416"/>
        <point val="10.1132,24.2393"/>
      </attribute>
      <attribute name="numcp">
        <int val="1"/>
      </attribute>
      <attribute name="line_width">
        <real val="0.01"/>
      </attribute>
    </object>
    <object type="Standard - Box" version="0" id="O17">
      <attribute name="obj_pos">
        <point val="15.4938,27.8096"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="15.4788,27.7946;19.5067,28.6379"/>
      </attribute>
      <attribute name="elem_corner">
        <point val="15.4938,27.8096"/>
      </attribute>
      <attribute name="elem_width">
        <real val="3.99788"/>
      </attribute>
      <attribute name="elem_height">
        <real val="0.813296"/>
      </attribute>
      <attribute name="border_width">
        <real val="0.03"/>
      </attribute>
      <attribute name="show_background">
        <boolean val="false"/>
      </attribute>
    </object>
    <object type="Standard - Line" version="0" id="O18">
      <attribute name="obj_pos">
        <point val="3.30264,6.81103"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="3.29764,6.80603;8.32885,6.81603"/>
      </attribute>
      <attribute name="conn_endpoints">
        <point val="3.30264,6.81103"/>
        <point val="8.32385,6.81103"/>
      </attribute>
      <attribute name="numcp">
        <int val="1"/>
      </attribute>
      <attribute name="line_width">
        <real val="0.01"/>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O19">
      <attribute name="obj_pos">
        <point val="5.81325,9.60668"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="3.69225,9.34264;7.93425,9.69264"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#178;&#188;&#181;&#173;&#164;&#206;&#164;&#200;&#164;&#170;&#164;&#234;&#192;&#193;&#181;&#225;&#164;&#183;&#164;&#222;&#164;&#185;&#161;&#163;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.35"/>
          </attribute>
          <attribute name="pos">
            <point val="5.81325,9.60668"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="1"/>
          </attribute>
          <attribute name="single">
            <real val="0.215635"/>
          </attribute>
          <attribute name="multi">
            <real val="0.17675"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O20">
      <attribute name="obj_pos">
        <point val="10.1166,14.4731"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="10.1166,14.1713;10.5206,14.5713"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#181;&#173;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="10.1166,14.4731"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Line" version="0" id="O21">
      <attribute name="obj_pos">
        <point val="14.8122,6.8275"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="14.8072,6.8225;14.8172,7.69884"/>
      </attribute>
      <attribute name="conn_endpoints">
        <point val="14.8122,6.8275"/>
        <point val="14.8122,7.69384"/>
      </attribute>
      <attribute name="numcp">
        <int val="1"/>
      </attribute>
      <attribute name="line_width">
        <real val="0.01"/>
      </attribute>
    </object>
    <object type="Standard - Line" version="0" id="O22">
      <attribute name="obj_pos">
        <point val="16.1672,6.85195"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="16.1622,6.84695;16.1725,7.68116"/>
      </attribute>
      <attribute name="conn_endpoints">
        <point val="16.1672,6.85195"/>
        <point val="16.1675,7.67616"/>
      </attribute>
      <attribute name="numcp">
        <int val="1"/>
      </attribute>
      <attribute name="line_width">
        <real val="0.01"/>
      </attribute>
    </object>
    <object type="Standard - Line" version="0" id="O23">
      <attribute name="obj_pos">
        <point val="17.5975,6.83427"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="17.5925,6.82927;17.6025,7.70561"/>
      </attribute>
      <attribute name="conn_endpoints">
        <point val="17.5975,6.83427"/>
        <point val="17.5975,7.70061"/>
      </attribute>
      <attribute name="numcp">
        <int val="1"/>
      </attribute>
      <attribute name="line_width">
        <real val="0.01"/>
      </attribute>
    </object>
  </layer>
  <layer name="&#202;&#184;&#187;&#250;" visible="true">
    <object type="Standard - Text" version="0" id="O24">
      <attribute name="obj_pos">
        <point val="4.30132,4.13126"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="4.30132,3.82951;9.2382,4.22951"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#190;&#227;&#179;&#178;&#188;&#212;(&#207;&#183;&#191;&#205;&#202;&#221;&#183;&#242;&#203;&#161;&#179;&#186;&#197;&#246;&#188;&#212;)#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="4.30132,4.13126"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O25">
      <attribute name="obj_pos">
        <point val="9.262,3.86961"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="9.262,3.56786;13.2535,3.96786"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#176;&#236;  &#201;&#244;  &#201;&#233;  &#195;&#180;  &#182;&#226;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="9.262,3.86961"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O26">
      <attribute name="obj_pos">
        <point val="9.1986,4.37146"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="9.1986,4.06971;13.6426,4.46971"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#207;&#183;&#191;&#205;&#203;&#172;&#204;&#228;&#180;&#199;&#184;&#238;&#180;&#240;&#203;&#220;&#205;&#248;&#205;&#209;&#206;&#193;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="9.1986,4.37146"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O27">
      <attribute name="obj_pos">
        <point val="13.9654,4.15165"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="13.9654,3.8499;16.3894,4.2499"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#192;&#193;&#181;&#225;&#189;&#241;&#193;&#237;&#179;&#231;&#201;&#188;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="13.9654,4.15165"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O28">
      <attribute name="obj_pos">
        <point val="1.47263,2.4897"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="1.47263,2.26338;3.35729,2.56338"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#(&#205;&#205;&#188;&#176;&#194;&#232;&#163;&#179;&#185;&#230;)#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.3"/>
          </attribute>
          <attribute name="pos">
            <point val="1.47263,2.4897"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.18483"/>
          </attribute>
          <attribute name="multi">
            <real val="0.1515"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O29">
      <attribute name="obj_pos">
        <point val="11.7733,5.80326"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="10.3997,5.42607;13.1469,5.92607"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#192;&#193;  &#181;&#225;  &#198;&#252;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.5"/>
          </attribute>
          <attribute name="pos">
            <point val="11.7733,5.80326"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="1"/>
          </attribute>
          <attribute name="single">
            <real val="0.30805"/>
          </attribute>
          <attribute name="multi">
            <real val="0.2525"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O30">
      <attribute name="obj_pos">
        <point val="11.7702,6.60258"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="10.3966,6.22539;13.1438,6.72539"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#191;&#199;  &#206;&#197;  &#183;&#238;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.5"/>
          </attribute>
          <attribute name="pos">
            <point val="11.7702,6.60258"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="1"/>
          </attribute>
          <attribute name="single">
            <real val="0.30805"/>
          </attribute>
          <attribute name="multi">
            <real val="0.2525"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O31">
      <attribute name="obj_pos">
        <point val="11.7702,7.4866"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="10.3411,7.10941;13.1994,7.60941"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#188;&#239;      &#202;&#204;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.5"/>
          </attribute>
          <attribute name="pos">
            <point val="11.7702,7.4866"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="1"/>
          </attribute>
          <attribute name="single">
            <real val="0.30805"/>
          </attribute>
          <attribute name="multi">
            <real val="0.2525"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O32">
      <attribute name="obj_pos">
        <point val="10.3766,8.2785"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="10.3766,7.97675;13.2046,8.37675"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#176;&#229;&#206;&#197;&#181;&#161;&#180;&#216;&#165;&#179;&#161;&#188;&#165;&#201;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="10.3766,8.2785"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O33">
      <attribute name="obj_pos">
        <point val="10.3144,9.25092"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="10.3144,8.94917;13.1424,12.5492"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#202;&#221;&#184;&#177;&#176;&#229;&#206;&#197;&#181;&#161;&#180;&#216;&#197;&#249;

  &#189;&#234;&#186;&#223;&#195;&#207;

  &#204;&#190;&#190;&#206;

  &#194;&#229;&#201;&#189;&#188;&#212;&#204;&#190;

  &#197;&#197;&#207;&#195;&#200;&#214;&#185;&#230;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="10.3144,9.25092"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O34">
      <attribute name="obj_pos">
        <point val="18.4572,11.9914"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="18.4572,11.7651;18.7602,12.0651"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#176;&#245;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.3"/>
          </attribute>
          <attribute name="pos">
            <point val="18.4572,11.9914"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.18483"/>
          </attribute>
          <attribute name="multi">
            <real val="0.1515"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O35">
      <attribute name="obj_pos">
        <point val="18.4041,12.0091"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="18.4041,11.7073;18.8081,12.1073"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#162;&#254;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="18.4041,12.0091"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O36">
      <attribute name="obj_pos">
        <point val="2.90342,15.7638"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="2.00654,15.462;3.8003,15.862"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#182;&#232;    &#202;&#172;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="2.90342,15.7638"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="1"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O37">
      <attribute name="obj_pos">
        <point val="7.02241,15.7424"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="5.75587,15.4406;8.28895,15.8406"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#183;&#239;       &#191;&#244;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="7.02241,15.7424"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="1"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O38">
      <attribute name="obj_pos">
        <point val="14.6619,15.7815"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="12.9025,15.4797;16.4213,15.8797"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#182;&#226;           &#179;&#219;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="14.6619,15.7815"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="1"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O39">
      <attribute name="obj_pos">
        <point val="18.8345,16.4003"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="18.8345,16.0985;19.2385,16.4985"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#177;&#223;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="18.8345,16.4003"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O40">
      <attribute name="obj_pos">
        <point val="9.61791,16.4142"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="9.61791,16.1124;10.0219,16.5124"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#183;&#239;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="9.61791,16.4142"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O41">
      <attribute name="obj_pos">
        <point val="2.87824,18.1447"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="1.98136,17.8429;3.77512,18.2429"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#192;&#193;    &#181;&#225;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="2.87824,18.1447"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="1"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O42">
      <attribute name="obj_pos">
        <point val="2.84288,22.2996"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="1.946,21.9978;3.73976,22.3978"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#183;&#232;    &#196;&#234;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="2.84288,22.2996"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="1"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O43">
      <attribute name="obj_pos">
        <point val="17.4881,28.3691"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="15.5792,28.0523;19.397,28.4723"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#196;&#243;&#189;&#208;&#192;&#232;&#161;&#167;&#185;&#241;&#202;&#221;&#207;&#162;&#185;&#231;&#178;&#241;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.42"/>
          </attribute>
          <attribute name="pos">
            <point val="17.4881,28.3691"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="1"/>
          </attribute>
          <attribute name="single">
            <real val="0.258762"/>
          </attribute>
          <attribute name="multi">
            <real val="0.2121"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O44">
      <attribute name="obj_pos">
        <point val="14.1051,7.41095"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="13.5779,7.1092;14.6323,7.5092"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#176;&#229; &#163;&#177;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="14.1051,7.41095"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="1"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O45">
      <attribute name="obj_pos">
        <point val="15.4951,7.4072"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="14.9679,7.10545;16.0223,7.50545"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#187;&#245; &#163;&#179;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="15.4951,7.4072"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="1"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O46">
      <attribute name="obj_pos">
        <point val="16.9121,7.4072"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="16.3849,7.10545;17.4393,7.50545"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#196;&#180; &#163;&#180;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="16.9121,7.4072"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="1"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O47">
      <attribute name="obj_pos">
        <point val="18.3278,7.42488"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="17.8006,7.12313;18.855,7.52313"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#203;&#172; &#163;&#185;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.4"/>
          </attribute>
          <attribute name="pos">
            <point val="18.3278,7.42488"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="1"/>
          </attribute>
          <attribute name="single">
            <real val="0.24644"/>
          </attribute>
          <attribute name="multi">
            <real val="0.202"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Standard - Text" version="0" id="O48">
      <attribute name="obj_pos">
        <point val="17.1199,6.57813"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="17.1199,6.23866;17.5744,6.68866"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#202;&#172;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.45"/>
          </attribute>
          <attribute name="pos">
            <point val="17.1199,6.57813"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.277245"/>
          </attribute>
          <attribute name="multi">
            <real val="0.22725"/>
          </attribute>
        </composite>
      </attribute>
    </object>
  </layer>
  <layer name="&#186;&#185;&#164;&#183;&#185;&#254;&#164;&#223;" visible="true">
    <object type="Embed - Text" version="0" id="O49">
      <attribute name="obj_pos">
        <point val="17.0282,6.58792"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="13.3922,6.24845;17.0282,6.69845"/>
      </attribute>
      <attribute name="dnode_path">
        <string>#SRYYM#</string>
      </attribute>
      <attribute name="embed_text_string">
        <enum val="0"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#202;&#191;&#192;&#174;&#163;&#185;&#163;&#185;&#199;&#175;&#163;&#185;&#163;&#185;&#183;&#238;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.45"/>
          </attribute>
          <attribute name="pos">
            <point val="17.0282,6.58792"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="2"/>
          </attribute>
          <attribute name="single">
            <real val="0.277245"/>
          </attribute>
          <attribute name="multi">
            <real val="0.22725"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Embed - Text" version="0" id="O50">
      <attribute name="obj_pos">
        <point val="13.338,9.93139"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="13.338,9.76543;18.893,10.2054"/>
      </attribute>
      <attribute name="dnode_path">
        <string>#ADRS#</string>
      </attribute>
      <attribute name="embed_text_column">
        <int val="50"/>
      </attribute>
      <attribute name="embed_text_string">
        <enum val="0"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;
&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.22"/>
          </attribute>
          <attribute name="pos">
            <point val="13.338,9.93139"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.135542"/>
          </attribute>
          <attribute name="multi">
            <real val="0.1111"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Embed - Text" version="0" id="O51">
      <attribute name="obj_pos">
        <point val="13.3621,10.8233"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="13.3621,10.6573;17.8061,11.0973"/>
      </attribute>
      <attribute name="dnode_path">
        <string>#HOSPNAME#</string>
      </attribute>
      <attribute name="embed_text_column">
        <int val="50"/>
      </attribute>
      <attribute name="embed_text_string">
        <enum val="0"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;
&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.22"/>
          </attribute>
          <attribute name="pos">
            <point val="13.3621,10.8233"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.135542"/>
          </attribute>
          <attribute name="multi">
            <real val="0.1111"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Embed - Text" version="0" id="O52">
      <attribute name="obj_pos">
        <point val="13.5151,11.9064"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="13.5151,11.6801;18.0601,11.9801"/>
      </attribute>
      <attribute name="dnode_path">
        <string>#KAISETU#</string>
      </attribute>
      <attribute name="embed_text_string">
        <enum val="0"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.3"/>
          </attribute>
          <attribute name="pos">
            <point val="13.5151,11.9064"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.18483"/>
          </attribute>
          <attribute name="multi">
            <real val="0.1515"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Embed - Text" version="0" id="O53">
      <attribute name="obj_pos">
        <point val="13.5872,8.29556"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="13.5872,7.94969;18.1322,8.39969"/>
      </attribute>
      <attribute name="dnode_path">
        <string>#HOSPCDN#</string>
      </attribute>
      <attribute name="embed_text_string">
        <enum val="0"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#163;&#177;&#163;&#178;&#163;&#179;&#163;&#180;&#163;&#181;&#163;&#182;&#163;&#183;&#163;&#184;&#163;&#185;&#163;&#176;#</string>
          </attribute>
          <attribute name="font">
            <font name="Times-Roman"/>
          </attribute>
          <attribute name="height">
            <real val="0.45"/>
          </attribute>
          <attribute name="pos">
            <point val="13.5872,8.29556"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.113625"/>
          </attribute>
          <attribute name="multi">
            <real val="0.22725"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Embed - Text" version="0" id="O54">
      <attribute name="obj_pos">
        <point val="18.3545,5.78324"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="13.355,5.44377;18.3545,5.89377"/>
      </attribute>
      <attribute name="dnode_path">
        <string>#SEIYMD#</string>
      </attribute>
      <attribute name="embed_text_string">
        <enum val="0"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#202;&#191;&#192;&#174;&#163;&#185;&#163;&#185;&#199;&#175;&#163;&#185;&#163;&#185;&#183;&#238;&#163;&#185;&#163;&#185;&#198;&#252;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.45"/>
          </attribute>
          <attribute name="pos">
            <point val="18.3545,5.78324"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="2"/>
          </attribute>
          <attribute name="single">
            <real val="0.277245"/>
          </attribute>
          <attribute name="multi">
            <real val="0.22725"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Embed - Text" version="0" id="O55">
      <attribute name="obj_pos">
        <point val="13.4915,7.51051"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="13.4915,7.01092;14.148,7.66092"/>
      </attribute>
      <attribute name="dnode_path">
        <string>#TENSUHYO[0]#</string>
      </attribute>
      <attribute name="embed_text_string">
        <enum val="0"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#162;&#254;#</string>
          </attribute>
          <attribute name="font">
            <font name="Times-Roman"/>
          </attribute>
          <attribute name="height">
            <real val="0.65"/>
          </attribute>
          <attribute name="pos">
            <point val="13.4915,7.51051"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.164125"/>
          </attribute>
          <attribute name="multi">
            <real val="0.32825"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Embed - Text" version="0" id="O56">
      <attribute name="obj_pos">
        <point val="14.8918,7.51288"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="14.8918,7.01329;15.5483,7.66329"/>
      </attribute>
      <attribute name="dnode_path">
        <string>#TENSUHYO[1]#</string>
      </attribute>
      <attribute name="embed_text_string">
        <enum val="0"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#162;&#254;#</string>
          </attribute>
          <attribute name="font">
            <font name="Times-Roman"/>
          </attribute>
          <attribute name="height">
            <real val="0.65"/>
          </attribute>
          <attribute name="pos">
            <point val="14.8918,7.51288"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.164125"/>
          </attribute>
          <attribute name="multi">
            <real val="0.32825"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Embed - Text" version="0" id="O57">
      <attribute name="obj_pos">
        <point val="16.2607,7.48056"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="16.2607,6.98097;16.9172,7.63097"/>
      </attribute>
      <attribute name="dnode_path">
        <string>#TENSUHYO[2]#</string>
      </attribute>
      <attribute name="embed_text_string">
        <enum val="0"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#162;&#254;#</string>
          </attribute>
          <attribute name="font">
            <font name="Times-Roman"/>
          </attribute>
          <attribute name="height">
            <real val="0.65"/>
          </attribute>
          <attribute name="pos">
            <point val="16.2607,7.48056"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.164125"/>
          </attribute>
          <attribute name="multi">
            <real val="0.32825"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Embed - Text" version="0" id="O58">
      <attribute name="obj_pos">
        <point val="17.7076,7.53056"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="17.7076,7.03097;18.3641,7.68097"/>
      </attribute>
      <attribute name="dnode_path">
        <string>#TENSUHYO[3]#</string>
      </attribute>
      <attribute name="embed_text_string">
        <enum val="0"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#162;&#254;#</string>
          </attribute>
          <attribute name="font">
            <font name="Times-Roman"/>
          </attribute>
          <attribute name="height">
            <real val="0.65"/>
          </attribute>
          <attribute name="pos">
            <point val="17.7076,7.53056"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.164125"/>
          </attribute>
          <attribute name="multi">
            <real val="0.32825"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Embed - Text" version="0" id="O59">
      <attribute name="obj_pos">
        <point val="8.41753,18.2197"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="5.82991,17.6916;8.41753,18.3916"/>
      </attribute>
      <attribute name="dnode_path">
        <string>#SEIKYU-AREA.KENSU#</string>
      </attribute>
      <attribute name="embed_text_string">
        <enum val="0"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#99,999#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.7"/>
          </attribute>
          <attribute name="pos">
            <point val="8.41753,18.2197"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="2"/>
          </attribute>
          <attribute name="single">
            <real val="0.43127"/>
          </attribute>
          <attribute name="multi">
            <real val="0.3535"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Embed - Text" version="0" id="O60">
      <attribute name="obj_pos">
        <point val="13.5192,12.8014"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="13.5192,12.4242;18.1399,12.9242"/>
      </attribute>
      <attribute name="dnode_path">
        <string>#TEL#</string>
      </attribute>
      <attribute name="embed_text_string">
        <enum val="0"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#123456789012345#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.5"/>
          </attribute>
          <attribute name="pos">
            <point val="13.5192,12.8014"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.30805"/>
          </attribute>
          <attribute name="multi">
            <real val="0.2525"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Embed - Text" version="0" id="O61">
      <attribute name="obj_pos">
        <point val="17.206,18.2241"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="12.462,17.696;17.206,18.396"/>
      </attribute>
      <attribute name="dnode_path">
        <string>#SEIKYU-AREA.SEIKYUGK#</string>
      </attribute>
      <attribute name="embed_text_string">
        <enum val="0"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#999,999,999#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.7"/>
          </attribute>
          <attribute name="pos">
            <point val="17.206,18.2241"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="2"/>
          </attribute>
          <attribute name="single">
            <real val="0.43127"/>
          </attribute>
          <attribute name="multi">
            <real val="0.3535"/>
          </attribute>
        </composite>
      </attribute>
    </object>
    <object type="Embed - Text" version="0" id="O62">
      <attribute name="obj_pos">
        <point val="3.41557,6.70745"/>
      </attribute>
      <attribute name="obj_bb">
        <rectangle val="3.41557,6.42833;8.27367,6.79833"/>
      </attribute>
      <attribute name="dnode_path">
        <string>#CITYNAME#</string>
      </attribute>
      <attribute name="embed_text_string">
        <enum val="0"/>
      </attribute>
      <attribute name="text">
        <composite type="text">
          <attribute name="string">
            <string>#&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;&#164;&#162;#</string>
          </attribute>
          <attribute name="font">
            <font name="Courier"/>
          </attribute>
          <attribute name="height">
            <real val="0.37"/>
          </attribute>
          <attribute name="pos">
            <point val="3.41557,6.70745"/>
          </attribute>
          <attribute name="color">
            <color val="#000000"/>
          </attribute>
          <attribute name="alignment">
            <enum val="0"/>
          </attribute>
          <attribute name="single">
            <real val="0.227957"/>
          </attribute>
          <attribute name="multi">
            <real val="0.18685"/>
          </attribute>
        </composite>
      </attribute>
    </object>
  </layer>
</diagram>
