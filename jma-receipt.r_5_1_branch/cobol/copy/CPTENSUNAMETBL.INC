      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *----------------------------------------------------------------*
      *    点数テーブル項目名称テーブル COPY (CPTENSUNAMETBL.INC)
      *----------------------------------------------------------------*
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       01  TBL-TENSU-NAME-AREA.
           03  FILLER                      PIC X(41)   VALUE
                       "001 srycd              診療行為コード    ".
           03  FILLER                      PIC X(41)   VALUE
                       "002 yukostymd          有効開始日        ".
           03  FILLER                      PIC X(41)   VALUE
                       "003 yukoedymd          有効終了日        ".
           03  FILLER                      PIC X(41)   VALUE
                       "004 srykbn             診療行為区分      ".
           03  FILLER                      PIC X(41)   VALUE
                       "005 srysyukbn          診療種別区分      ".
           03  FILLER                      PIC X(41)   VALUE
                       "006 yukoketa           漢字名称桁数      ".
           03  FILLER                      PIC X(41)   VALUE
                       "007 name               漢字名称          ".
           03  FILLER                      PIC X(41)   VALUE
                       "008 kanayukoketa       カナ名称桁数      ".
           03  FILLER                      PIC X(41)   VALUE
                       "009 kananame           カナ名称          ".
           03  FILLER                      PIC X(41)   VALUE
                       "010 formalyukoketa     正式名称桁数      ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "011 formalname         正式名称          ".
           03  FILLER                      PIC X(41)   VALUE
                       "012 tensikibetu        点数識別          ".
           03  FILLER                      PIC X(41)   VALUE
                       "013 ten                点数／金額        ".
           03  FILLER                      PIC X(41)   VALUE
                       "014 tanicd             単位コード        ".
           03  FILLER                      PIC X(41)   VALUE
                       "015 tanimojisu         単位名称桁数      ".
           03  FILLER                      PIC X(41)   VALUE
                       "016 taniname           単位名称          ".
           03  FILLER                      PIC X(41)   VALUE
                       "017 datakbn            データ区分        ".
           03  FILLER                      PIC X(41)   VALUE
                       "018 hkntekkbn          保険適用区分      ".
           03  FILLER                      PIC X(41)   VALUE
                       "019 nyugaitekkbn       入外適用区分      ".
           03  FILLER                      PIC X(41)   VALUE
                       "020 routekkbn          後期高齢者適用区分".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "021 hospsrykbn         病院・診療所区分  ".
           03  FILLER                      PIC X(41)   VALUE
                       "022 osinkbn            往診区分（未使用）".
           03  FILLER                      PIC X(41)   VALUE
                       "023 houksnkbn          包括対象検査      ".
           03  FILLER                      PIC X(41)   VALUE
                       "024 byokanrenkbn       傷病名関連区分    ".
           03  FILLER                      PIC X(41)   VALUE
                       "025 sdokanryo          医学管理料        ".
           03  FILLER                      PIC X(41)   VALUE
                       "026 jituday            実日数            ".
           03  FILLER                      PIC X(41)   VALUE
                       "027 daycnt             日数・回数        ".
           03  FILLER                      PIC X(41)   VALUE
                       "028 ykzknrkbn          医薬品関連区分    ".
           03  FILLER                      PIC X(41)   VALUE
                       "029 kzmcompsikibetu    きざみ値計算識別  ".
           03  FILLER                      PIC X(41)   VALUE
                       "030 kzmkgn             きざみ値下限値    ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "031 kzmjgn             きざみ値上限値    ".
           03  FILLER                      PIC X(41)   VALUE
                       "032 kzm                きざみ値          ".
           03  FILLER                      PIC X(41)   VALUE
                       "033 kzmten             きざみ点数        ".
           03  FILLER                      PIC X(41)   VALUE
                       "034 kzmerr             きざみ値エラー処理".
           03  FILLER                      PIC X(41)   VALUE
                       "035 jgncnt             上限回数１月      ".
           03  FILLER                      PIC X(41)   VALUE
                       "036 jgncnt1d           上限回数１日      ".
           03  FILLER                      PIC X(41)   VALUE
                       "037 jgncnt1w           上限回数１週      ".
           03  FILLER                      PIC X(41)   VALUE
                       "038 jgncntetcm         上限回数他月数    ".
           03  FILLER                      PIC X(41)   VALUE
                       "039 jgncntetc          上限回数他月      ".
           03  FILLER                      PIC X(41)   VALUE
                       "040 jgncnterr          上限回数エラー処理".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "041 chuksncd           注加算コード      ".
           03  FILLER                      PIC X(41)   VALUE
                       "042 chuksntsuban       注加算通番        ".
           03  FILLER                      PIC X(41)   VALUE
                       "043 tsusokuage         通則年齢          ".
           03  FILLER                      PIC X(41)   VALUE
                       "044 kgnage             下限年齢          ".
           03  FILLER                      PIC X(41)   VALUE
                       "045 jgnage             上限年齢          ".
           03  FILLER                      PIC X(41)   VALUE
                       "046 timeksnkbn         時間加算区分      ".
           03  FILLER                      PIC X(41)   VALUE
                       "047 kijunteigenkbn     基準適合区分      ".
           03  FILLER                      PIC X(41)   VALUE
                       "048 kijunteigencd      基準適合施設基準  ".
           03  FILLER                      PIC X(41)   VALUE
                       "049 laserksn           処置乳幼児加算区分".
           03  FILLER                      PIC X(41)   VALUE
                       "050 chpmesuksn         極低出生体重児加算".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "051 micmesuksn         入院基本料等減算  ".
           03  FILLER                      PIC X(41)   VALUE
                       "052 donorkbn           ドナー分集計区分  ".
           03  FILLER                      PIC X(41)   VALUE
                       "053 knsjiskbn          検査等実施判断区分".
           03  FILLER                      PIC X(41)   VALUE
                       "054 knsjisgrpkbn       検査等実施判断Ｇ  ".
           03  FILLER                      PIC X(41)   VALUE
                       "055 teigenkbn          逓減対象区分      ".
           03  FILLER                      PIC X(41)   VALUE
                       "056 sekizuisokutei     脊髄誘発電位測定  ".
           03  FILLER                      PIC X(41)   VALUE
                       "057 keibujyutu         頸部郭清術併施加算".
           03  FILLER                      PIC X(41)   VALUE
                       "058 autohougo          自動縫合器加算区分".
           03  FILLER                      PIC X(41)   VALUE
                       "059 gaikanrikbn        外来管理加算区分  ".
           03  FILLER                      PIC X(41)   VALUE
                       "060 tusokugaikbn       通則加算所定点数  ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "061 hokatuteigenkbn    包括逓減区分      ".
           03  FILLER                      PIC X(41)   VALUE
                       "062 chpnaisiksn        超音波内視鏡加算  ".
           03  FILLER                      PIC X(41)   VALUE
                       "063 autofungo          自動吻合器加算区分".
           03  FILLER                      PIC X(41)   VALUE
                       "064 chiikiksn          地域加算          ".
           03  FILLER                      PIC X(41)   VALUE
                       "065 byosyokbn          病床数区分        ".
           03  FILLER                      PIC X(41)   VALUE
                       "066 chpgyokosotiksn    超音波凝固切開措置".
           03  FILLER                      PIC X(41)   VALUE
                       "067 shortstayope       短期滞在手術      ".
           03  FILLER                      PIC X(41)   VALUE
                       "068 buikbn             部位区分          ".
           03  FILLER                      PIC X(41)   VALUE
                       "069 santeirrkkbn       算定履歴区分      ".
           03  FILLER                      PIC X(41)   VALUE
                       "070 sstkijuncd1        施設基準１        ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "071 sstkijuncd2        施設基準２        ".
           03  FILLER                      PIC X(41)   VALUE
                       "072 sstkijuncd3        施設基準３        ".
           03  FILLER                      PIC X(41)   VALUE
                       "073 sstkijuncd4        施設基準４        ".
           03  FILLER                      PIC X(41)   VALUE
                       "074 sstkijuncd5        施設基準５        ".
           03  FILLER                      PIC X(41)   VALUE
                       "075 sstkijuncd6        施設基準６        ".
           03  FILLER                      PIC X(41)   VALUE
                       "076 sstkijuncd7        施設基準７        ".
           03  FILLER                      PIC X(41)   VALUE
                       "077 sstkijuncd8        施設基準８        ".
           03  FILLER                      PIC X(41)   VALUE
                       "078 sstkijuncd9        施設基準９        ".
           03  FILLER                      PIC X(41)   VALUE
                       "079 sstkijuncd10       施設基準１０      ".
           03  FILLER                      PIC X(41)   VALUE
                       "080 ageksnkgn1         年齢加算下限年齢１".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "081 ageksnjgn1         年齢加算上限年齢１".
           03  FILLER                      PIC X(41)   VALUE
                       "082 ageksnsrycd1       年齢加算コード１  ".
           03  FILLER                      PIC X(41)   VALUE
                       "083 ageksnkgn2         年齢加算下限年齢２".
           03  FILLER                      PIC X(41)   VALUE
                       "084 ageksnjgn2         年齢加算上限年齢２".
           03  FILLER                      PIC X(41)   VALUE
                       "085 ageksnsrycd2       年齢加算コード２  ".
           03  FILLER                      PIC X(41)   VALUE
                       "086 ageksnkgn3         年齢加算下限年齢３".
           03  FILLER                      PIC X(41)   VALUE
                       "087 ageksnjgn3         年齢加算上限年齢３".
           03  FILLER                      PIC X(41)   VALUE
                       "088 ageksnsrycd3       年齢加算コード３  ".
           03  FILLER                      PIC X(41)   VALUE
                       "089 ageksnkgn4         年齢加算下限年齢４".
           03  FILLER                      PIC X(41)   VALUE
                       "090 ageksnjgn4         年齢加算上限年齢４".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "091 ageksnsrycd4       年齢加算コード４  ".
           03  FILLER                      PIC X(41)   VALUE
                       "092 kentaicomment      検体検査コメント  ".
           03  FILLER                      PIC X(41)   VALUE
                       "093 nyukhnkbn          入院基本料区分    ".
           03  FILLER                      PIC X(41)   VALUE
                       "094 nyukhnksnkbn       入院基本料加算区分".
           03  FILLER                      PIC X(41)   VALUE
                       "095 kangoksn           看護加算          ".
           03  FILLER                      PIC X(41)   VALUE
                       "096 oldtenskb          旧点数識別        ".
           03  FILLER                      PIC X(41)   VALUE
                       "097 oldten             旧点数／金額      ".
           03  FILLER                      PIC X(41)   VALUE
                       "098 madokukbn          麻薬・毒薬等区分  ".
           03  FILLER                      PIC X(41)   VALUE
                       "099 sinkeihakaikbn     神経破壊剤        ".
           03  FILLER                      PIC X(41)   VALUE
                       "100 seibutugakukbn     生物学的製剤      ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "101 zoeizaikbn         造影（補助）剤    ".
           03  FILLER                      PIC X(41)   VALUE
                       "102 csyyouryo          注射容量          ".
           03  FILLER                      PIC X(41)   VALUE
                       "103 ykzkbn             薬剤区分          ".
           03  FILLER                      PIC X(41)   VALUE
                       "104 zaigatakbn         剤形              ".
           03  FILLER                      PIC X(41)   VALUE
                       "105 kouhatukbn         後発品            ".
           03  FILLER                      PIC X(41)   VALUE
                       "106 longtoyokbn        長期投与（未使用）".
           03  FILLER                      PIC X(41)   VALUE
                       "107 meiuseskb          名称使用識別      ".
           03  FILLER                      PIC X(41)   VALUE
                       "108 tokukizaiageksnkbn 年齢加算区分      ".
           03  FILLER                      PIC X(41)   VALUE
                       "109 sansokbn           酸素等区分        ".
           03  FILLER                      PIC X(41)   VALUE
                       "110 tokukizaisbt1      特定器材種別      ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "111 tokukizaisbt2      上限価格          ".
           03  FILLER                      PIC X(41)   VALUE
                       "112 jgnten             上限点数          ".
           03  FILLER                      PIC X(41)   VALUE
                       "113 gaitenttlkbn       点数欄集計先・外来".
           03  FILLER                      PIC X(41)   VALUE
                       "114 nyutenttlkbn       点数欄集計先・入院".
           03  FILLER                      PIC X(41)   VALUE
                       "115 cdkbn_kbn          コード表・区分    ".
           03  FILLER                      PIC X(41)   VALUE
                       "116 cdkbn_syo          コード表・章      ".
           03  FILLER                      PIC X(41)   VALUE
                       "117 cdkbn_bu           コード表・部      ".
           03  FILLER                      PIC X(41)   VALUE
                       "118 cdkbn_kbnnum       コード表・区分番号".
           03  FILLER                      PIC X(41)   VALUE
                       "119 cdkbn_kbnnum_eda   コード表・枝番    ".
           03  FILLER                      PIC X(41)   VALUE
                       "120 cdkbn_kouban       コード表・項番    ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "121 kokuchi_kbn        告知通知・区分    ".
           03  FILLER                      PIC X(41)   VALUE
                       "122 kokuchi_syo        告知通知・章      ".
           03  FILLER                      PIC X(41)   VALUE
                       "123 kokuchi_bu         告知通知・部      ".
           03  FILLER                      PIC X(41)   VALUE
                       "124 kokuchi_kbnnum     告知通知・区分番号".
           03  FILLER                      PIC X(41)   VALUE
                       "125 kokuchi_kbnnum_eda 告知通知・枝番    ".
           03  FILLER                      PIC X(41)   VALUE
                       "126 kokuchi_kouban     告知通知・項番    ".
           03  FILLER                      PIC X(41)   VALUE
                       "127 kokujiskbkbn1      告示等識別区分１  ".
           03  FILLER                      PIC X(41)   VALUE
                       "128 kokujiskbkbn2      告示等識別区分２  ".
           03  FILLER                      PIC X(41)   VALUE
                       "129 namechgkbn         漢字名称変更区分  ".
           03  FILLER                      PIC X(41)   VALUE
                       "130 kananamechgkbn     カナ名称変更区分  ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "131 idokanren          異動関連          ".
           03  FILLER                      PIC X(41)   VALUE
                       "132 kouhyojyunnum      公表順序番号      ".
           03  FILLER                      PIC X(41)   VALUE
                       "133 yakkakjncd         薬価基準コード    ".
           03  FILLER                      PIC X(41)   VALUE
                       "134 syusaiskb          収戴方式等識別    ".
           03  FILLER                      PIC X(41)   VALUE
                       "135 syomeikanren       商品名等関連      ".
           03  FILLER                      PIC X(41)   VALUE
                       "136 chgymd             変更年月日        ".
           03  FILLER                      PIC X(41)   VALUE
                       "137 haisiymd           廃止年月日        ".
           03  FILLER                      PIC X(41)   VALUE
                       "138 keikasochiymd      経過措置年月日    ".
           03  FILLER                      PIC X(41)   VALUE
                       "139 termid             端末ＩＤ          ".
           03  FILLER                      PIC X(41)   VALUE
                       "140 opid               オペレーターＩＤ  ".
      *
           03  FILLER                      PIC X(41)   VALUE
                       "141 creymd             作成年月日        ".
           03  FILLER                      PIC X(41)   VALUE
                       "142 upymd              更新年月日        ".
           03  FILLER                      PIC X(41)   VALUE
                       "143 uphms              更新時間          ".
           03  FILLER                      PIC X(41)   VALUE
                       "144 hospnum            医療機関識別番号  ".
           03  FILLER                      PIC X(41)   VALUE
                       "145 gazoopesup         画像等手術支援加算".
           03  FILLER                      PIC X(41)   VALUE
                       "146 iryokansatukbn     医療観察法対象区分".
           03  FILLER                      PIC X(41)   VALUE
                       "147 masuiskbkbn        麻酔識別区分      ".
      *H24.4追加４つ
           03  FILLER                      PIC X(41)   VALUE
                       "148 fukubikunaikbn     副鼻腔内視鏡      ".
           03  FILLER                      PIC X(41)   VALUE
                       "149 fukubikukotukbn    副鼻腔骨軟部      ".
           03  FILLER                      PIC X(41)   VALUE
                       "150 timemasuikbn       長時間麻酔        ".
           03  FILLER                      PIC X(41)   VALUE
                       "151 dpckbn             ＤＰＣ区分        ".
      *H28.4追加５つ
           03  FILLER                      PIC X(41)   VALUE
                       "152 hisinsyumonitorksn 非侵襲的血行動態モ".
           03  FILLER                      PIC X(41)   VALUE
                       "153 touketuhozonksn    凍結保存同種組織加".
           03  FILLER                      PIC X(41)   VALUE
                       "154 kubunbangou        点数表区分番号    ".
           03  FILLER                      PIC X(41)   VALUE
                       "155 rosaikbn           労災算定不可      ".
           03  FILLER                      PIC X(41)   VALUE
                       "156 sisiksn            四肢加算（労災）  ".
      *H30.4追加１つ
           03  FILLER                      PIC X(41)   VALUE
                       "157 akuseibyoriksn     悪性腫瘍病理組織  ".
      *R02.4追加３つ
           03  FILLER                      PIC X(41)   VALUE
                       "158 sogaikoteiksn      創外固定器加算    ".
           03  FILLER                      PIC X(41)   VALUE
                       "159 chpsessakuksn      超音波切削器加算  ".
           03  FILLER                      PIC X(41)   VALUE
                       "160 sasinjiheisikbn    左心耳閉鎖術併施区".
      *
       01  TBL-TENSU-NAME-AREA-R   REDEFINES   TBL-TENSU-NAME-AREA.
           03  TBL-TENSU-NAME-OCC  OCCURS  160 INDEXED  TBL-TNM.
               05  TBL-TNM-NUM                 PIC 9(03).
               05  TBL-TNM-FILLER1             PIC X(01).
               05  TBL-TNM-CNAME               PIC X(18).
               05  TBL-TNM-FILLER2             PIC X(01).
               05  TBL-TNM-NAME                PIC X(18).
      *
       01  TBL-TENSU-NAME-MAX          PIC 9(04)  VALUE  160.
