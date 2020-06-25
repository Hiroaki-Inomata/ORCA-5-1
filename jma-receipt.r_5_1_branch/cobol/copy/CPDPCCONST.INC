      ********************************************************************
      *    ＤＰＣ  診療科区分・出来高テーブル
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.00.00    NACL-伊藤    12/05/09  ２０１２年診療報酬改定
      *                                     特定入院料等の包括算定分記録
      *****************************************************************
      *
      *    レセ電診療科コード→ＤＰＣ診療科区分対応テーブル
       01  TBL-DPCSRYKACD-AREA.
               05  FILLER          PIC X(02)   VALUE   "01". 
               05  FILLER          PIC X(03)   VALUE   "010".
               05  FILLER          PIC X(02)   VALUE   "02". 
               05  FILLER          PIC X(03)   VALUE   "030".
               05  FILLER          PIC X(02)   VALUE   "03". 
               05  FILLER          PIC X(03)   VALUE   "040".
               05  FILLER          PIC X(02)   VALUE   "04". 
               05  FILLER          PIC X(03)   VALUE   "280".
               05  FILLER          PIC X(02)   VALUE   "05". 
               05  FILLER          PIC X(03)   VALUE   "050".
               05  FILLER          PIC X(02)   VALUE   "06". 
               05  FILLER          PIC X(03)   VALUE   "060".
               05  FILLER          PIC X(02)   VALUE   "07". 
               05  FILLER          PIC X(03)   VALUE   "290".
               05  FILLER          PIC X(02)   VALUE   "08". 
               05  FILLER          PIC X(03)   VALUE   "070".
               05  FILLER          PIC X(02)   VALUE   "09". 
               05  FILLER          PIC X(03)   VALUE   "100".
               05  FILLER          PIC X(02)   VALUE   "10". 
               05  FILLER          PIC X(03)   VALUE   "110".
               05  FILLER          PIC X(02)   VALUE   "11". 
               05  FILLER          PIC X(03)   VALUE   "120".
               05  FILLER          PIC X(02)   VALUE   "12". 
               05  FILLER          PIC X(03)   VALUE   "130".
               05  FILLER          PIC X(02)   VALUE   "13". 
               05  FILLER          PIC X(03)   VALUE   "140".
               05  FILLER          PIC X(02)   VALUE   "14". 
               05  FILLER          PIC X(03)   VALUE   "150".
               05  FILLER          PIC X(02)   VALUE   "15". 
               05  FILLER          PIC X(03)   VALUE   "160".
               05  FILLER          PIC X(02)   VALUE   "16". 
               05  FILLER          PIC X(03)   VALUE   "170".
               05  FILLER          PIC X(02)   VALUE   "17". 
               05  FILLER          PIC X(03)   VALUE   "180".
               05  FILLER          PIC X(02)   VALUE   "18". 
               05  FILLER          PIC X(03)   VALUE   "190".
               05  FILLER          PIC X(02)   VALUE   "19". 
               05  FILLER          PIC X(03)   VALUE   "300".
               05  FILLER          PIC X(02)   VALUE   "20". 
               05  FILLER          PIC X(03)   VALUE   "310".
               05  FILLER          PIC X(02)   VALUE   "21". 
               05  FILLER          PIC X(03)   VALUE   "200".
               05  FILLER          PIC X(02)   VALUE   "22". 
               05  FILLER          PIC X(03)   VALUE   "210".
               05  FILLER          PIC X(02)   VALUE   "23". 
               05  FILLER          PIC X(03)   VALUE   "220".
               05  FILLER          PIC X(02)   VALUE   "24". 
               05  FILLER          PIC X(03)   VALUE   "320".
               05  FILLER          PIC X(02)   VALUE   "25". 
               05  FILLER          PIC X(03)   VALUE   "330".
               05  FILLER          PIC X(02)   VALUE   "26". 
               05  FILLER          PIC X(03)   VALUE   "230".
               05  FILLER          PIC X(02)   VALUE   "27". 
               05  FILLER          PIC X(03)   VALUE   "240".
               05  FILLER          PIC X(02)   VALUE   "28". 
               05  FILLER          PIC X(03)   VALUE   "250".
               05  FILLER          PIC X(02)   VALUE   "30". 
               05  FILLER          PIC X(03)   VALUE   "270".
               05  FILLER          PIC X(02)   VALUE   "31". 
               05  FILLER          PIC X(03)   VALUE   "490".
               05  FILLER          PIC X(02)   VALUE   "33". 
               05  FILLER          PIC X(03)   VALUE   "020".
               05  FILLER          PIC X(02)   VALUE   "34". 
               05  FILLER          PIC X(03)   VALUE   "080".
               05  FILLER          PIC X(02)   VALUE   "35". 
               05  FILLER          PIC X(03)   VALUE   "090".
               05  FILLER          PIC X(02)   VALUE   "36". 
               05  FILLER          PIC X(03)   VALUE   "260".
      *
       01  TBL-DPCSRYKACD-AREA-R   REDEFINES   TBL-DPCSRYKACD-AREA.
           03  TBL-DPCSRYKACD-OCC  OCCURS      34
                                       INDEXED TBL-DPCSRYKACD-IDX.
               05  TBL-RECEDEN-SRYKACD PIC X(02).
               05  TBL-DPC-SRYKACD     PIC X(03).
      *
       01  TBL-DPCSRYKACD-MAX  PIC 9(02)   VALUE   34.
      *
      *
      *
      *
       01  TBL-DPCDEKIDAKA-AREA.
               05  FILLER          PIC X(09)   VALUE   "190024310".
               05  FILLER          PIC X(09)   VALUE   "190024410".
               05  FILLER          PIC X(09)   VALUE   "190024510".
               05  FILLER          PIC X(09)   VALUE   "190024610".
               05  FILLER          PIC X(09)   VALUE   "190024710".
               05  FILLER          PIC X(09)   VALUE   "190024810".
               05  FILLER          PIC X(09)   VALUE   "190066710".
               05  FILLER          PIC X(09)   VALUE   "190066810".
               05  FILLER          PIC X(09)   VALUE   "190074510".
               05  FILLER          PIC X(09)   VALUE   "190074610".
               05  FILLER          PIC X(09)   VALUE   "190075510".
               05  FILLER          PIC X(09)   VALUE   "190075610".
               05  FILLER          PIC X(09)   VALUE   "190075810".
               05  FILLER          PIC X(09)   VALUE   "190110210".
               05  FILLER          PIC X(09)   VALUE   "190110310".
               05  FILLER          PIC X(09)   VALUE   "190110410".
               05  FILLER          PIC X(09)   VALUE   "190110510".
               05  FILLER          PIC X(09)   VALUE   "190110610".
               05  FILLER          PIC X(09)   VALUE   "190110710".
               05  FILLER          PIC X(09)   VALUE   "190110810".
               05  FILLER          PIC X(09)   VALUE   "190110910".
               05  FILLER          PIC X(09)   VALUE   "190111070".
               05  FILLER          PIC X(09)   VALUE   "190111110".
               05  FILLER          PIC X(09)   VALUE   "190111210".
               05  FILLER          PIC X(09)   VALUE   "190111370".
               05  FILLER          PIC X(09)   VALUE   "190116310".
               05  FILLER          PIC X(09)   VALUE   "190116410".
               05  FILLER          PIC X(09)   VALUE   "190117310".
               05  FILLER          PIC X(09)   VALUE   "190120810".
      *
       01  TBL-DPCDEKIDAKA-AREA-R  REDEFINES   TBL-DPCDEKIDAKA-AREA.
           03  TBL-DPCDEKIDAKA-OCC OCCURS      29
                                       INDEXED TBL-DPCDEKIDAKA-IDX.
               05  TBL-DPCDEKIDAKA-SRYCD PIC X(09).
      *
       01  TBL-DPCDEKIDAKA-SRYCD-MAX     PIC 9(02)   VALUE   29.
      *
      *
      *
      *平成２６年３月までの判定用
       01  TBL-TOKUTEINYUIN22-AREA.
      * 2012.04 診療報酬改定
      * A300 救命救急入院料１（３日以内）
               05  FILLER          PIC X(09)   VALUE   "190024510".
      * A300 救命救急入院料１（４日以上７日以内）
               05  FILLER          PIC X(09)   VALUE   "190128610".
      * A300 救命救急入院料１（８日以上１４日以内）
               05  FILLER          PIC X(09)   VALUE   "190074510".
      * A300 救命救急入院料１（３日以内）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190714410".
      * A300 救命救急入院料１（４〜７日）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190714510".
      * A300 救命救急入院料１（８〜１４日）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190714610".
      * A300 救命救急入院料２（３日以内）
               05  FILLER          PIC X(09)   VALUE   "190024310".
      * A300 救命救急入院料２（４日以上７日以内）
               05  FILLER          PIC X(09)   VALUE   "190128710".
      * A300 救命救急入院料２（８日以上１４日以内）
               05  FILLER          PIC X(09)   VALUE   "190024410".
      * A300 救命救急入院料２（３日以内）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190714710".
      *(11)
      * A300 救命救急入院料２（４〜７日）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190714810".
      * A300 救命救急入院料２（８〜１４日）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190714910".
      * A300 救命救急入院料３（救命救急入院料）（３日以内）
               05  FILLER          PIC X(09)   VALUE   "190138110".
      * A300 救命救急入院料３（救命救急入院料）（４日以上７日以内）
               05  FILLER          PIC X(09)   VALUE   "190138210".
      * A300 救命救急入院料３（救命救急入院料）（８日以上１４日以内）
               05  FILLER          PIC X(09)   VALUE   "190138310".
      * A300 救命救急入院料３（救命救急）（３日以内）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190715010".
      * A300 救命救急入院料３（救命救急）（４〜７日）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190715110".
      * A300 救命救急入院料３（救命救急）（８〜１４日）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190715210".
      * A300 救命救急入院料３（広範囲熱傷特定集中治療・３日以内）
               05  FILLER          PIC X(09)   VALUE   "190138410".
      * A300 救命救急入院料３（広範囲熱傷特定集中治療・４日〜７日）
               05  FILLER          PIC X(09)   VALUE   "190138510".
      *(21)
      * A300 救命救急入院料３（広範囲熱傷特定集中治療・８日〜６０日）
               05  FILLER          PIC X(09)   VALUE   "190138610".
      * A300 救命救急入院料３（広範囲熱傷・３日以内）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190715310".
      * A300 救命救急入院料３（広範囲熱傷・４〜７日）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190715410".
      * A300 救命救急入院料３（広範囲熱傷・８〜６０日）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190715510".
      * A300 救命救急入院料４（救命救急入院料）（３日以内）
               05  FILLER          PIC X(09)   VALUE   "190138710".
      * A300 救命救急入院料４（救命救急入院料）（４日以上７日以内）
               05  FILLER          PIC X(09)   VALUE   "190138810".
      * A300 救命救急入院料４（救命救急入院料）（８日以上１４日以内）
               05  FILLER          PIC X(09)   VALUE   "190138910".
      * A300 救命救急入院料４（救命救急入院料）（３日以内）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190715610".
      * A300 救命救急入院料４（救命救急入院料）（４〜７日）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190715710".
      * A300 救命救急入院料４（救命救急入院料）（８〜１４日）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190715810".
      *(31)
      * A300 救命救急入院料４（広範囲熱傷特定集中治療・３日以内）
               05  FILLER          PIC X(09)   VALUE   "190139010".
      * A300 救命救急入院料４（広範囲熱傷特定集中治療・４日〜７日）
               05  FILLER          PIC X(09)   VALUE   "190139110".
      * A300 救命救急入院料４（広範囲熱傷特定集中治療・８日〜１４日）
               05  FILLER          PIC X(09)   VALUE   "190139210".
      * A300 救命救急入院料４（広範囲熱傷特定集中治療・１５日〜６０日）
               05  FILLER          PIC X(09)   VALUE   "190139310".
      * A300 救命救急入院料４（広範囲熱傷・３日以内）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190715910".
      * A300 救命救急入院料４（広範囲熱傷・４〜７日）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190716010".
      * A300 救命救急入院料４（広範囲熱傷・８〜１４日）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190716110".
      * A300 救命救急入院料４（広範囲熱傷・１５〜６０日）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190716210".
      * A301 特定集中治療室管理料１（７日以内）
               05  FILLER          PIC X(09)   VALUE   "190116310".
      * A301 特定集中治療室管理料１（８日以上１４日以内）
               05  FILLER          PIC X(09)   VALUE   "190116410".
      *(41)
      * A301 特定集中治療室管理料１（７日以内）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190717210".
      * A301 特定集中治療室管理料１（８日以上１４日以内）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190717310".
      * A301 特定集中治療室管理料２（特定集中治療室管理料・７日以内）
               05  FILLER          PIC X(09)   VALUE   "190139810".
      * A301 特定集中治療室管理料２（特定集中治療室管理料・８日〜１４日）
               05  FILLER          PIC X(09)   VALUE   "190139910".
      * A301 特定集中治療室管理料２（特定集中・７日以内）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190717410".
      * A301 特定集中治療室管理料２（特定集中・８〜１４日）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190717510".
      * A301 特定集中治療室管理料２（広範囲熱傷特定集中治療・７日以内）
               05  FILLER          PIC X(09)   VALUE   "190140010".
      * A301 特定集中治療室管理料２（広範囲熱傷特定集中治療・８日〜６０日）
               05  FILLER          PIC X(09)   VALUE   "190140110".
      * A301 特定集中治療室管理料２（広範囲熱傷・７日以内）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190717610".
      * A301 特定集中治療室管理料２（広範囲熱傷・８〜６０日）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190717710".
      *(51)
      * A301-2 ハイケアユニット入院医療管理料
               05  FILLER          PIC X(09)   VALUE   "190117310".
      * A301-2 ハイケアユニット入院医療管理料（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190717810".
      * A301-3 脳卒中ケアユニット入院医療管理料
               05  FILLER          PIC X(09)   VALUE   "190120810".
      * A301-3 脳卒中ケアユニット入院医療管理料（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190717910".
      * A301-4 小児特定集中治療室管理料（７日以内）
               05  FILLER          PIC X(09)   VALUE   "190149910".
      * A301-4 小児特定集中治療室管理料（８日以上１４日以内）
               05  FILLER          PIC X(09)   VALUE   "190150010".
      * A301-4 小児特定集中治療室管理料（７日以内）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190718010".
      * A301-4 小児特定集中治療室管理料（８日以上１４日以内）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190718110".
      * A302 新生児特定集中治療室管理料１
               05  FILLER          PIC X(09)   VALUE   "190024710".
      * A302 新生児特定集中治療室管理料１（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190718210".
      *(61)
      * A302 新生児特定集中治療室管理料２
               05  FILLER          PIC X(09)   VALUE   "190140410".
      * A302 新生児特定集中治療室管理料２（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190718310".
      * A303 総合周産期特定集中治療室管理料（母体・胎児）
               05  FILLER          PIC X(09)   VALUE   "190066710".
      * A303 総合周産期特定集中治療室管理料（母体・胎児）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190718410".
      * A303 総合周産期特定集中治療室管理料（新生児）
               05  FILLER          PIC X(09)   VALUE   "190066810".
      * A303 総合周産期特定集中治療室管理料（新生児）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190718510".
      * A303-2 新生児治療回復室入院医療管理料
               05  FILLER          PIC X(09)   VALUE   "190140510".
      * A303-2 新生児治療回復室入院医療管理料（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190718610".
      * A305 一類感染症患者入院医療管理料（７日以内）
               05  FILLER          PIC X(09)   VALUE   "190075510".
      * A305 一類感染症患者入院医療管理料（８日以上１４日以内）
               05  FILLER          PIC X(09)   VALUE   "190075610".
      *(71)
      * A305 一類感染症患者入院医療管理料（７日以内）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190718710".
      * A305 一類感染症患者入院医療管理料（８日〜１４日以内）（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190718810".
      * A307 小児入院医療管理料１
               05  FILLER          PIC X(09)   VALUE   "190129310".
      * A307 小児入院医療管理料２
               05  FILLER          PIC X(09)   VALUE   "190140710".
      * A307 小児入院医療管理料３
               05  FILLER          PIC X(09)   VALUE   "190111110".
      * A307 小児入院医療管理料４
               05  FILLER          PIC X(09)   VALUE   "190111210".
      * A307 小児入院医療管理料５
               05  FILLER          PIC X(09)   VALUE   "190075810".
      * A307 小児入院医療管理料１（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190719010".
      * A307 小児入院医療管理料２（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190719110".
      * A307 小児入院医療管理料３（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190719210".
      *(81)
      * A307 小児入院医療管理料４（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190719310".
      * A307 小児入院医療管理料５（栄管経過措置）
               05  FILLER          PIC X(09)   VALUE   "190719410".
      * A400 3 短期滞在手術基本料３
               05  FILLER          PIC X(09)   VALUE   "190130410".
      * A400 3 短期滞在手術基本料３（生活療養）
               05  FILLER          PIC X(09)   VALUE   "190130510".
      *
       01  TBL-TOKUTEINYUIN22-AREA-R  REDEFINES TBL-TOKUTEINYUIN22-AREA.
           03  TBL-TOKUTEINYUIN22-OCC OCCURS      84
                                       INDEXED TBL-TOKUTEINYUIN22-IDX.
               05  TBL-TOKUTEINYUIN22-SRYCD PIC X(09).
      *
      *平成２６年４月からの判定用(2014)
       01  TBL-TOKUTEINYUIN26-AREA.
      * 2012.04 診療報酬改定
      * A300 救命救急入院料１（３日以内）
               05  FILLER          PIC X(09)   VALUE   "190024510".
      * A300 救命救急入院料１（４日以上７日以内）
               05  FILLER          PIC X(09)   VALUE   "190128610".
      * A300 救命救急入院料１（８日以上１４日以内）
               05  FILLER          PIC X(09)   VALUE   "190074510".
      * A300 救命救急入院料２（３日以内）
               05  FILLER          PIC X(09)   VALUE   "190024310".
      * A300 救命救急入院料２（４日以上７日以内）
               05  FILLER          PIC X(09)   VALUE   "190128710".
      * A300 救命救急入院料２（８日以上１４日以内）
               05  FILLER          PIC X(09)   VALUE   "190024410".
      * A300 救命救急入院料３（救命救急入院料）（３日以内）
               05  FILLER          PIC X(09)   VALUE   "190138110".
      * A300 救命救急入院料３（救命救急入院料）（４日以上７日以内）
               05  FILLER          PIC X(09)   VALUE   "190138210".
      * A300 救命救急入院料３（救命救急入院料）（８日以上１４日以内）
               05  FILLER          PIC X(09)   VALUE   "190138310".
      * A300 救命救急入院料３（広範囲熱傷特定集中治療・３日以内）
               05  FILLER          PIC X(09)   VALUE   "190138410".
      *(11)
      * A300 救命救急入院料３（広範囲熱傷特定集中治療・４日〜７日）
               05  FILLER          PIC X(09)   VALUE   "190138510".
      * A300 救命救急入院料３（広範囲熱傷特定集中治療・８日〜６０日）
               05  FILLER          PIC X(09)   VALUE   "190138610".
      * A300 救命救急入院料４（救命救急入院料）（３日以内）
               05  FILLER          PIC X(09)   VALUE   "190138710".
      * A300 救命救急入院料４（救命救急入院料）（４日以上７日以内）
               05  FILLER          PIC X(09)   VALUE   "190138810".
      * A300 救命救急入院料４（救命救急入院料）（８日以上１４日以内）
               05  FILLER          PIC X(09)   VALUE   "190138910".
      * A300 救命救急入院料４（広範囲熱傷特定集中治療・３日以内）
               05  FILLER          PIC X(09)   VALUE   "190139010".
      * A300 救命救急入院料４（広範囲熱傷特定集中治療・４日〜７日）
               05  FILLER          PIC X(09)   VALUE   "190139110".
      * A300 救命救急入院料４（広範囲熱傷特定集中治療・８日〜１４日）
               05  FILLER          PIC X(09)   VALUE   "190139210".
      * A300 救命救急入院料４（広範囲熱傷特定集中治療・１５日〜６０日）
               05  FILLER          PIC X(09)   VALUE   "190139310".
      * A301 特定集中治療室管理料１（７日以内）
               05  FILLER          PIC X(09)   VALUE   "190174410".
      *(21)
      * A301 特定集中治療室管理料１（８日以上１４日以内）
               05  FILLER          PIC X(09)   VALUE   "190174510".
      * A301 特定集中治療室管理料２（特定集中治療室管理料・７日以内）
               05  FILLER          PIC X(09)   VALUE   "190174610".
      * A301 特定集中治療室管理料２（特定集中治療室管理料・８日〜１４日）
               05  FILLER          PIC X(09)   VALUE   "190174710".
      * A301 特定集中治療室管理料２（広範囲熱傷特定集中治療・７日以内）
               05  FILLER          PIC X(09)   VALUE   "190174810".
      * A301 特定集中治療室管理料２（広範囲熱傷特定集中治療・８日〜６０日）
               05  FILLER          PIC X(09)   VALUE   "190174910".
      * A301 特定集中治療室管理料３（７日以内）
               05  FILLER          PIC X(09)   VALUE   "190116310".
      * A301 特定集中治療室管理料３（８日以上１４日以内）
               05  FILLER          PIC X(09)   VALUE   "190116410".
      * A301 特定集中治療室管理料４（特定集中治療室管理料・７日以内）
               05  FILLER          PIC X(09)   VALUE   "190139810".
      * A301 特定集中治療室管理料４（特定集中治療室管理料・８日〜１４日）
               05  FILLER          PIC X(09)   VALUE   "190139910".
      * A301 特定集中治療室管理料４（広範囲熱傷特定集中治療・７日以内）
               05  FILLER          PIC X(09)   VALUE   "190140010".
      *(31)
      * A301 特定集中治療室管理料４（広範囲熱傷特定集中治療・８日〜６０日）
               05  FILLER          PIC X(09)   VALUE   "190140110".
      * A301-2 ハイケアユニット入院医療管理料１
               05  FILLER          PIC X(09)   VALUE   "190175010".
      * A301-2 ハイケアユニット入院医療管理料２
               05  FILLER          PIC X(09)   VALUE   "190175110".
      * A301-2 ハイケアユニット入院医療管理料（経過措置）
               05  FILLER          PIC X(09)   VALUE   "190117310".
      * A301-3 脳卒中ケアユニット入院医療管理料
               05  FILLER          PIC X(09)   VALUE   "190120810".
      * A301-4 小児特定集中治療室管理料（７日以内）
               05  FILLER          PIC X(09)   VALUE   "190149910".
      * A301-4 小児特定集中治療室管理料（８日以上１４日以内）
               05  FILLER          PIC X(09)   VALUE   "190150010".
      * A302 新生児特定集中治療室管理料１
               05  FILLER          PIC X(09)   VALUE   "190024710".
      * A302 新生児特定集中治療室管理料２
               05  FILLER          PIC X(09)   VALUE   "190140410".
      * A303 総合周産期特定集中治療室管理料（母体・胎児）
               05  FILLER          PIC X(09)   VALUE   "190066710".
      *(41)
      * A303 総合周産期特定集中治療室管理料（新生児）
               05  FILLER          PIC X(09)   VALUE   "190066810".
      * A303-2 新生児治療回復室入院医療管理料
               05  FILLER          PIC X(09)   VALUE   "190140510".
      * A305 一類感染症患者入院医療管理料（７日以内）
               05  FILLER          PIC X(09)   VALUE   "190075510".
      * A305 一類感染症患者入院医療管理料（８日以上１４日以内）
               05  FILLER          PIC X(09)   VALUE   "190075610".
      * A307 小児入院医療管理料１
               05  FILLER          PIC X(09)   VALUE   "190129310".
      * A307 小児入院医療管理料２
               05  FILLER          PIC X(09)   VALUE   "190140710".
      * A307 小児入院医療管理料３
               05  FILLER          PIC X(09)   VALUE   "190111110".
      * A307 小児入院医療管理料４
               05  FILLER          PIC X(09)   VALUE   "190111210".
      * A307 小児入院医療管理料５
               05  FILLER          PIC X(09)   VALUE   "190075810".
      * A400  短手３（終夜睡眠ポリグラフィー１（携帯用装置使用））
               05  FILLER          PIC X(09)   VALUE   "190177610".
      *(51)
      * A400  短手３（終夜睡眠ポリグラフィー１（携帯用装置使用））（生活）
               05  FILLER          PIC X(09)   VALUE   "190177710".
      * A400  短手３（終夜睡眠ポリグラフィー２（多点感圧センサ））
               05  FILLER          PIC X(09)   VALUE   "190177810".
      * A400  短手３（終夜睡眠ポリグラフィー２（多点感圧センサ））（生活）
               05  FILLER          PIC X(09)   VALUE   "190177910".
      * A400  短手３（終夜睡眠ポリグラフィー３（１及び２以外））
               05  FILLER          PIC X(09)   VALUE   "190178010".
      * A400  短手３（終夜睡眠ポリグラフィー３（１及び２以外））（生活）
               05  FILLER          PIC X(09)   VALUE   "190178110".
      * A400  短手３（小児食物アレルギー負荷検査）
               05  FILLER          PIC X(09)   VALUE   "190178210".
      * A400  短手３（小児食物アレルギー負荷検査）（生活）
               05  FILLER          PIC X(09)   VALUE   "190178310".
      * A400  短手３（前立腺針生検法）
               05  FILLER          PIC X(09)   VALUE   "190178410".
      * A400  短手３（前立腺針生検法）（生活）
               05  FILLER          PIC X(09)   VALUE   "190178510".
      * A400  短手３（腋臭症手術（皮膚有毛部切除術））
               05  FILLER          PIC X(09)   VALUE   "190178610".
      *(61)
      * A400  短手３（腋臭症手術（皮膚有毛部切除術））（生活）
               05  FILLER          PIC X(09)   VALUE   "190178710".
      * A400  短手３（関節鏡下手根管開放手術）
               05  FILLER          PIC X(09)   VALUE   "190178810".
      * A400  短手３（関節鏡下手根管開放手術）（生活）
               05  FILLER          PIC X(09)   VALUE   "190178910".
      * A400  短手３（胸腔鏡下交感神経節切除術（両側））
               05  FILLER          PIC X(09)   VALUE   "190179010".
      * A400  短手３（胸腔鏡下交感神経節切除術（両側））（生活）
               05  FILLER          PIC X(09)   VALUE   "190179110".
      * A400  短手３（水晶体再建術・眼内レンズ挿入・その他のもの）
               05  FILLER          PIC X(09)   VALUE   "190179210".
      * A400  短手３（水晶体再建術・眼内レンズ挿入・その他のもの）（生活）
               05  FILLER          PIC X(09)   VALUE   "190179310".
      * A400  短手３（水晶体再建術（眼内レンズ挿入しない場合））
               05  FILLER          PIC X(09)   VALUE   "190179410".
      * A400  短手３（水晶体再建術（眼内レンズ挿入しない場合））（生活）
               05  FILLER          PIC X(09)   VALUE   "190179510".
      * A400  短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））
               05  FILLER          PIC X(09)   VALUE   "190179610".
      *(71)
      * A400  短手３（乳腺腫瘍摘出術（長径５ｃｍ未満））（生活）
               05  FILLER          PIC X(09)   VALUE   "190179710".
      * A400  短手３（下肢静脈瘤手術（抜去切除術））
               05  FILLER          PIC X(09)   VALUE   "190179810".
      * A400  短手３（下肢静脈瘤手術（抜去切除術））（生活）
               05  FILLER          PIC X(09)   VALUE   "190179910".
      * A400  短手３（下肢静脈瘤手術（硬化療法））
               05  FILLER          PIC X(09)   VALUE   "190180010".
      * A400  短手３（下肢静脈瘤手術（硬化療法））（生活）
               05  FILLER          PIC X(09)   VALUE   "190180110".
      * A400  短手３（下肢静脈瘤手術（高位結紮術））
               05  FILLER          PIC X(09)   VALUE   "190180210".
      * A400  短手３（下肢静脈瘤手術（高位結紮術））（生活）
               05  FILLER          PIC X(09)   VALUE   "190180310".
      * A400  短手３（鼠径ヘルニア手術（１５歳未満））
               05  FILLER          PIC X(09)   VALUE   "190180410".
      * A400  短手３（鼠径ヘルニア手術（１５歳未満））（生活）
               05  FILLER          PIC X(09)   VALUE   "190180510".
      * A400  短手３（鼠径ヘルニア手術（１５歳以上））
               05  FILLER          PIC X(09)   VALUE   "190180610".
      *(81)
      * A400  短手３（鼠径ヘルニア手術（１５歳以上））（生活）
               05  FILLER          PIC X(09)   VALUE   "190180710".
      * A400  短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳未満））
               05  FILLER          PIC X(09)   VALUE   "190180810".
      * A400  短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳未満））（生活）
               05  FILLER          PIC X(09)   VALUE   "190180910".
      * A400  短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））
               05  FILLER          PIC X(09)   VALUE   "190181010".
      * A400  短手３（腹腔鏡下鼠径ヘルニア手術）（１５歳以上））（生活）
               05  FILLER          PIC X(09)   VALUE   "190181110".
      * A400  短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）
               05  FILLER          PIC X(09)   VALUE   "190181210".
      * A400  短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ未満）（生活）
               05  FILLER          PIC X(09)   VALUE   "190181310".
      * A400  短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）
               05  FILLER          PIC X(09)   VALUE   "190181410".
      * A400  短手３（内視鏡的結腸ポリープ・粘膜切除術２ｃｍ以上）（生活）
               05  FILLER          PIC X(09)   VALUE   "190181510".
      * A400  短手３（痔核手術（硬化療法（四段階注射法））
               05  FILLER          PIC X(09)   VALUE   "190181610".
      *(91)
      * A400  短手３（痔核手術（硬化療法（四段階注射法））（生活）
               05  FILLER          PIC X(09)   VALUE   "190181710".
      * A400  短手３（子宮頚部（腟部）切除術）
               05  FILLER          PIC X(09)   VALUE   "190181810".
      * A400  短手３（子宮頚部（腟部）切除術）（生活）
               05  FILLER          PIC X(09)   VALUE   "190181910".
      * A400  短手３（子宮鏡下子宮筋腫摘出術）
               05  FILLER          PIC X(09)   VALUE   "190182010".
      * A400  短手３（子宮鏡下子宮筋腫摘出術）（生活）
               05  FILLER          PIC X(09)   VALUE   "190182110".
      * A400  短手３（水晶体再建術・水晶体嚢拡張リング・縫着なし）
               05  FILLER          PIC X(09)   VALUE   "190182750".
      * A400  短手３（水晶体再建術・水晶体嚢拡張リング・縫着なし）（生活）
               05  FILLER          PIC X(09)   VALUE   "190182850".
      *
       01  TBL-TOKUTEINYUIN26-AREA-R  REDEFINES TBL-TOKUTEINYUIN26-AREA.
           03  TBL-TOKUTEINYUIN26-OCC OCCURS      97
                                       INDEXED TBL-TOKUTEINYUIN26-IDX.
               05  TBL-TOKUTEINYUIN26-SRYCD PIC X(09).
      *
       01  TBL-TOKUTEINYUIN-SRYCD-MAX     PIC 9(02)   VALUE   84.
