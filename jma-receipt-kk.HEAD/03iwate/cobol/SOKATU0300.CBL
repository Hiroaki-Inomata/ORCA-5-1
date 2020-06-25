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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             SOKATU0300.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国保・請求書集計処理
      *  管理者            :
      *  作成日付    作業者        記述
      *  05/06/20    山口          ORCAのバージョンアップ(2.5.0)による再作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev 	修正者	日付		内容
      *  01.00.01     劉丹   2006/02/11  国保総括表の返戻対応の修正 
      *  01.00.02     中川   2006/08/29  請求先名(県内)追加63→64 
      *  01.00.03     内田&  2006/10/29  帳票様式の変更に伴う修正
      *  01.00.04     内田   2006/11/15  退職７割の集計の修正
      *                                  食事がない場合でも生活療養費を印字
      *  01.00.05     内田   2007/07/13  国組で前期高齢者の集計先不具合の修正
      *  01.00.06     内田   2007/08/19  日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *  01.00.07     長屋   08/04/27    ４月改正に伴う修正
      *  01.00.08     門間   2008/10/09  SPA-AREAの初期化
      *  01.00.09     吉川   2008/10/22  在総診以外の在宅は在医総とする
      *  01.00.10     吉川   2009/07/03  在総診と在医総の2つがある場合、
      *                                  在総診とするように修正
      *  01.00.11     吉川   2010/03/11  帳票様式の変更に伴う修正
      *  01.00.12     吉川   2010/07/01  オンライン返戻対応
      *  01.00.13     吉川   2011/04/04  東北地方太平洋沖地震対応
      *  01.00.14     吉川   2011/06/15  特別療養費国保対応
      *  01.00.15     吉川   2012/04/19  帳票様式の変更に伴う修正
      *  01.00.16     吉川   2012/12/19  医療機関情報編集修正
      *  01.00.17     吉川   2014/05/20  前期高齢者２割対応
      *                                  在宅の対象とする診療コード変更
      *  01.00.18     吉川   2014/08/06  返戻時、再印刷画面のタイトル修正
      *  01.00.19     吉川   2014/10/03  ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.20     吉川   2016/04/14  医療機関編集情報への開設者名追加対応
      *  01.00.21     吉川   2016/05/16  在宅コード変更（平成２８年４月改正）
      *  01.00.22     門間   2017/01/11  医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *  01.00.23     萩野   2017/03/08  医療機関情報の取得の修正
      *  01.00.24     門間   2018/04/09  SITEKBN変更（動作に変更なし）
      *  01.00.25     井上   2018/10/10  修正履歴コメント削除
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    請求書一時ファイル
           SELECT  SKT0300TMP-FILE ASSIGN    SKT0300TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT0300TMP-KEY
                                   FILE    STATUS  IS  STS-SKT0300.
      *
      *    岩手保険者データ
           SELECT  HKNJA-FILE     ASSIGN FILE-NAME
                                 ORGANIZATION   IS  LINE SEQUENTIAL
                                 ACCESS MODE    IS  SEQUENTIAL
                                 FILE   STATUS  IS  STS-TOWN.
      *
006900*    請求ＤＢ更新用ファイル
           SELECT  UPD-FILE        ASSIGN  RECEUPD
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-RECEUPD.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  SKT0300TMP-FILE.
           COPY    "CPSKT0300TMP.INC".
      *
      *    岩手保険者データ
       FD  HKNJA-FILE.
           01  HKNJA-REC.
               03  HKNJA-DAT-NUM        PIC 9(02).
               03  HKNJA-DAT-SP1        PIC 9(01).
               03  HKNJA-DAT-HKNJANUM   PIC 9(06).
               03  HKNJA-DAT-SP2        PIC 9(01).
               03  HKNJA-DAT-NAME       PIC X(16).
      *
006900*    請求ＤＢ更新用ファイル
       FD  UPD-FILE.
           COPY    "CPRCF010.INC"  REPLACING  //RECE10//
                                   BY         //RECE10X//.
      *
006900*    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    請求書一時ファイル 名称領域
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //SKT0300TP//.
      *
006900*    請求ＤＢ更新用ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEUPD//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *
      *    パラメータファイル名
       01  FILE-NAME        PIC X(512).
      *
      *    ファイル名
       01  DATA-FILE.
           03  FILLER       PIC X(05)  VALUE  "data/".
           03  FILLER       PIC X(13)  VALUE  "HKNJA0300.TXT".
      *
      *    岩手保険者データ
           COPY  "HKNJA0300.INC".
      *
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *    請求書一時データ
           COPY    "SKT0300.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT0300         PIC X(02).
           03  STS-TOWN            PIC X(02).
           03  STS-RECE10          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT0300TMP      PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PASS            PIC 9(01).
           03  FLG-ZAIISO          PIC 9(01).
           03  FLG-ZAIISO2         PIC 9(01).
           03  FLG-TOWN            PIC 9(01).
           03  FLG-HKNJACNT        PIC 9(01).
           03  FLG-KOUKI           PIC 9(01).
           03  FLG-ZAISOSIN        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
      *
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-TOKUBETU   PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SEIYMD          PIC X(08).
           03  WRK-SEIYMDWH        PIC X(22).
           03  WRK-SRYYMWH         PIC X(16).
           03  WRK-SRYYMD          PIC X(08).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-CNT             PIC 9(02).
           03  WRK-HKNJANUM        PIC 9(06).
      *
           03  WRK-Z2              PIC ZZ.
           03  WRK-Z3              PIC ZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
      *
           03  WRK-HKNJASU-TBL  OCCURS  300.
               05  WRK-HKNJASU     PIC 9(06).
      *
           03  WRK-RECEERR         PIC X(200).
           03  WRK-PATH            PIC X(50).
           03  WRK-PATH2               PIC X(50).
           03  WRK-HKNNUM              PIC X(03).
      *    医療機関情報
           03  WRK-HOSPNAME        PIC X(100).
           03  WRK-HOSPNAME-R      REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1   PIC X(50).
               05  WRK-HOSPNAME2   PIC X(50).
           03  WRK-HOSPADRS        PIC X(100).
           03  WRK-HOSPADRS-R      REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1   PIC X(50).
               05  WRK-HOSPADRS2   PIC X(50).
           03  WRK-HOSPTEL         PIC X(15).
           03  WRK-KAISETUNAME     PIC X(80).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
           03  CNT-KEN             PIC 9(02).
      *
      *在総診テーブル
       01  TBL-ZAITAKU-AREA.
      *    在宅総合・在宅末期
           03  CONS-ZAITAKU1       PIC X(09) VALUE "114007610".
           03  CONS-ZAITAKU2       PIC X(09) VALUE "114007710".
           03  CONS-ZAITAKU3       PIC X(09) VALUE "114007510".
           03  CONS-ZAITAKU4       PIC X(09) VALUE "114012210".
           03  CONS-ZAITAKU5       PIC X(09) VALUE "114012310".
           03  CONS-ZAITAKU6       PIC X(09) VALUE "114012410".
      *
      *在総診テーブル（２０１４年４月診療分〜）
       01  TBL-ZAISOSIN-AREA.
      *    在がん医総（在支診等）
           03  FILLER                  PIC X(09) VALUE "114007610".
           03  FILLER                  PIC X(09) VALUE "114007710".
      *    在がん医総（機能強化した在支診等）
           03  FILLER                  PIC X(09) VALUE "114019510".
           03  FILLER                  PIC X(09) VALUE "114019610".
           03  FILLER                  PIC X(09) VALUE "114019710".
           03  FILLER                  PIC X(09) VALUE "114019810".
      *
       01  TBL-ZAISOSIN-R  REDEFINES   TBL-ZAISOSIN-AREA.
           03  TBL-ZAISOSIN            PIC X(09) OCCURS  6.
      *
       01  TBL-ZAISOSIN-MAX            PIC 9(02) VALUE   6.
      *
      *在医総テーブル（２０１４年４月診療分〜）
       01  TBL-ZAIISO-AREA.
      *    在医総管（機能強化在支診等）
           03  FILLER                  PIC X(09) VALUE "114023110".
           03  FILLER                  PIC X(09) VALUE "114023210".
           03  FILLER                  PIC X(09) VALUE "114023310".
           03  FILLER                  PIC X(09) VALUE "114023410".
           03  FILLER                  PIC X(09) VALUE "114023510".
           03  FILLER                  PIC X(09) VALUE "114023610".
           03  FILLER                  PIC X(09) VALUE "114023710".
           03  FILLER                  PIC X(09) VALUE "114023810".
      *    在医総管（在支診等）
           03  FILLER                  PIC X(09) VALUE "114023910".
           03  FILLER                  PIC X(09) VALUE "114024010".
           03  FILLER                  PIC X(09) VALUE "114024110".
           03  FILLER                  PIC X(09) VALUE "114024210".
      *    在医総管（在支診等以外）
           03  FILLER                  PIC X(09) VALUE "114024310".
           03  FILLER                  PIC X(09) VALUE "114024410".
           03  FILLER                  PIC X(09) VALUE "114024510".
           03  FILLER                  PIC X(09) VALUE "114024610".
      *
       01  TBL-ZAIISO-R  REDEFINES   TBL-ZAIISO-AREA.
           03  TBL-ZAIISO              PIC X(09) OCCURS  16.
      *
       01  TBL-ZAIISO-MAX              PIC 9(02) VALUE   16.
      *    在総診テーブル（２０１６年４月診療分〜）
       01  TBL-ZAISOSIN2-AREA.
      *    在がん医総（在支診等）
           03  FILLER          PIC X(09)   VALUE   "114007610".
           03  FILLER          PIC X(09)   VALUE   "114007710".
      *    在がん医総（機能強化した在支診等）
           03  FILLER          PIC X(09)   VALUE   "114019510".
           03  FILLER          PIC X(09)   VALUE   "114019610".
           03  FILLER          PIC X(09)   VALUE   "114019710".
           03  FILLER          PIC X(09)   VALUE   "114019810".
      *
       01  TBL-ZAISOSIN2-R  REDEFINES   TBL-ZAISOSIN2-AREA.
           03  TBL-ZAISOSIN2            PIC X(09) OCCURS  6.
      *
       01  TBL-ZAISOSIN2-MAX            PIC 9(02) VALUE   6.
      *
      *    在医総テーブル（２０１６年４月診療分〜）
       01  TBL-ZAIISO2-AREA.
      *    在医総管（機能強化在支診等）
           03  FILLER          PIC X(09)   VALUE   "114030710".
           03  FILLER          PIC X(09)   VALUE   "114030810".
           03  FILLER          PIC X(09)   VALUE   "114030910".
           03  FILLER          PIC X(09)   VALUE   "114031010".
           03  FILLER          PIC X(09)   VALUE   "114031110".
           03  FILLER          PIC X(09)   VALUE   "114031210".
           03  FILLER          PIC X(09)   VALUE   "114031310".
           03  FILLER          PIC X(09)   VALUE   "114031410".
           03  FILLER          PIC X(09)   VALUE   "114031510".
           03  FILLER          PIC X(09)   VALUE   "114031610".
           03  FILLER          PIC X(09)   VALUE   "114031710".
           03  FILLER          PIC X(09)   VALUE   "114031810".
           03  FILLER          PIC X(09)   VALUE   "114031910".
           03  FILLER          PIC X(09)   VALUE   "114032010".
           03  FILLER          PIC X(09)   VALUE   "114032110".
           03  FILLER          PIC X(09)   VALUE   "114032210".
           03  FILLER          PIC X(09)   VALUE   "114032310".
           03  FILLER          PIC X(09)   VALUE   "114032410".
      *    在医総管（在支診等）
           03  FILLER          PIC X(09)   VALUE   "114032510".
           03  FILLER          PIC X(09)   VALUE   "114032610".
           03  FILLER          PIC X(09)   VALUE   "114032710".
           03  FILLER          PIC X(09)   VALUE   "114032810".
           03  FILLER          PIC X(09)   VALUE   "114032910".
           03  FILLER          PIC X(09)   VALUE   "114033010".
           03  FILLER          PIC X(09)   VALUE   "114033110".
           03  FILLER          PIC X(09)   VALUE   "114033210".
           03  FILLER          PIC X(09)   VALUE   "114033310".
      *    在医総管（在支診等以外）
           03  FILLER          PIC X(09)   VALUE   "114033410".
           03  FILLER          PIC X(09)   VALUE   "114033510".
           03  FILLER          PIC X(09)   VALUE   "114033610".
           03  FILLER          PIC X(09)   VALUE   "114033710".
           03  FILLER          PIC X(09)   VALUE   "114033810".
           03  FILLER          PIC X(09)   VALUE   "114033910".
           03  FILLER          PIC X(09)   VALUE   "114034010".
           03  FILLER          PIC X(09)   VALUE   "114034110".
           03  FILLER          PIC X(09)   VALUE   "114034210".
      *    施医総管（機能強化在支診等）
           03  FILLER          PIC X(09)   VALUE   "114035510".
           03  FILLER          PIC X(09)   VALUE   "114035610".
           03  FILLER          PIC X(09)   VALUE   "114035710".
           03  FILLER          PIC X(09)   VALUE   "114035810".
           03  FILLER          PIC X(09)   VALUE   "114035910".
           03  FILLER          PIC X(09)   VALUE   "114036010".
           03  FILLER          PIC X(09)   VALUE   "114036110".
           03  FILLER          PIC X(09)   VALUE   "114036210".
           03  FILLER          PIC X(09)   VALUE   "114036310".
           03  FILLER          PIC X(09)   VALUE   "114036410".
           03  FILLER          PIC X(09)   VALUE   "114036510".
           03  FILLER          PIC X(09)   VALUE   "114036610".
           03  FILLER          PIC X(09)   VALUE   "114036710".
           03  FILLER          PIC X(09)   VALUE   "114036810".
           03  FILLER          PIC X(09)   VALUE   "114036910".
           03  FILLER          PIC X(09)   VALUE   "114037010".
           03  FILLER          PIC X(09)   VALUE   "114037110".
           03  FILLER          PIC X(09)   VALUE   "114037210".
      *    施医総管（在支診等）
           03  FILLER          PIC X(09)   VALUE   "114037310".
           03  FILLER          PIC X(09)   VALUE   "114037410".
           03  FILLER          PIC X(09)   VALUE   "114037510".
           03  FILLER          PIC X(09)   VALUE   "114037610".
           03  FILLER          PIC X(09)   VALUE   "114037710".
           03  FILLER          PIC X(09)   VALUE   "114037810".
           03  FILLER          PIC X(09)   VALUE   "114037910".
           03  FILLER          PIC X(09)   VALUE   "114038010".
           03  FILLER          PIC X(09)   VALUE   "114038110".
      *    施医総管（在支診等以外）
           03  FILLER          PIC X(09)   VALUE   "114038210".
           03  FILLER          PIC X(09)   VALUE   "114038310".
           03  FILLER          PIC X(09)   VALUE   "114038410".
           03  FILLER          PIC X(09)   VALUE   "114038510".
           03  FILLER          PIC X(09)   VALUE   "114038610".
           03  FILLER          PIC X(09)   VALUE   "114038710".
           03  FILLER          PIC X(09)   VALUE   "114038810".
           03  FILLER          PIC X(09)   VALUE   "114038910".
           03  FILLER          PIC X(09)   VALUE   "114039010".
      *
       01  TBL-ZAIISO2-R  REDEFINES   TBL-ZAIISO2-AREA.
           03  TBL-ZAIISO2              PIC X(09) OCCURS  72.
      *
       01  TBL-ZAIISO2-MAX              PIC 9(02) VALUE   72.
           COPY    "COMMON-SPA".
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
           03  WRK-CONS-SRYYM-201404   PIC X(06)   VALUE   "201404".
           03  WRK-CONS-SRYYM-201604   PIC X(06)   VALUE   "201604".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *
      *    医療機関住所編集情報（２）
           COPY    "CPSK1903.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      * 
      ******************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SKT0300TMP-REC
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
           DISPLAY  "START"
      *
      *    市町村情報
           PERFORM 110-TOWNINF-GET-SEC
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID    
                                               LNK-PRTKANRI-PRTNM   
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-SYOKBN
                                               WRK-PARA-TOKUBETU
                                               WRK-PARA-STSRYYM
                                               WRK-PARA-EDSRYYM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SOKATU0300"    TO  JOB-PGID
           MOVE    "国保総括表"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "SKT0300"           TO  SKT0300TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT0300TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT0300TP-HOSPNUM
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  SKT0300TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT0300TP-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEERR
      *
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SRYYMD (7:2)
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYMWH 
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    LNK-PRTKANRI-SKYYMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO 
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-SEIYMDWH
           END-IF
      *
      *    医療機関ＩＤ編集
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
                                           SYS-1001-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               IF      SYS-1001-HOSPNAME   =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-HOSPNAME
               END-IF    
               IF      SYS-1001-KAISETUNAME    =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1001-KAISETUNAME
               END-IF    
           ELSE
               MOVE    "医療機関情報が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1002-STYUKYMD(7:2)
                                           SYS-1002-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC        TO  SYS-1002-REC
               IF      SYS-1002-ADRS       =   LOW-VALUE
                   MOVE    SPACE               TO  SYS-1002-ADRS
               END-IF
           ELSE
               MOVE    "医療機関情報（所在地）が取得できませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
           OPEN    OUTPUT  UPD-FILE
      *
           EVALUATE    WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
               WHEN   "2"               ALSO  "1"
                   MOVE    "key37"        TO  WRK-PATH
                   MOVE    "key65"        TO  WRK-PATH2
               WHEN   "1"               ALSO  "1"
                   MOVE    "key38"        TO  WRK-PATH
                   MOVE    "key66"        TO  WRK-PATH2
               WHEN   "2"               ALSO  "2"
                   MOVE    "key39"        TO  WRK-PATH
                   MOVE    "key65"        TO  WRK-PATH2
               WHEN   "1"               ALSO  "2"
                   MOVE    "key40"        TO  WRK-PATH
                   MOVE    "key66"        TO  WRK-PATH2
           END-EVALUATE
           PERFORM 900-SEIKYU-SELECT-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN     OUTPUT  SKT0300TMP-FILE
           CLOSE            SKT0300TMP-FILE
           OPEN     I-O     SKT0300TMP-FILE
      *
           MOVE     ZERO    TO      FLG-END
           MOVE     ZERO    TO      FLG-UPD
      *
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   UPD-FILE
           CLOSE   SKT0300TMP-FILE
      *
           IF       FLG-UPD   =       1
                    PERFORM   2002-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了    処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF      CNT-RECE10          =   ZERO 
                   CONTINUE
           ELSE
                   IF      FLG-UPD     =   1
                           PERFORM     310-SEIKYU-UPD-SEC
                   END-IF
           END-IF
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SOKATU0300 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU0300 END ***"
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA

           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号別集計処理
      *****************************************************************
       2001-KOKUHO-SYUKEI-SEC              SECTION.
      *
           INITIALIZE       SKT0300TMP-REC
      *
      *    TMPキーセット
      *
           PERFORM  800-SKTTMP-READ-SEC
      *
           IF  RECE10-HKNJANUM(7:2)  =  SPACE
             MOVE  RECE10-HKNJANUM(1:6)  TO  WRK-HKNJANUM
           ELSE
             MOVE  RECE10-HKNJANUM(3:6)  TO  WRK-HKNJANUM
           END-IF
      *
           IF  RECE10-HKNJANUM  =  ZERO
             MOVE  999999                TO  WRK-HKNJANUM
           END-IF
      *
      *    保険者数（既にカウントした保険者か）
           INITIALIZE  FLG-HKNJACNT
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     300
                    OR       WRK-HKNJASU(IDX1)  =  ZERO
             IF  WRK-HKNJANUM  =  WRK-HKNJASU(IDX1)
               MOVE  1    TO  FLG-HKNJACNT
               MOVE  300  TO  IDX1
             END-IF
           END-PERFORM
           IF  IDX1 < 300
             MOVE  WRK-HKNJANUM  TO  WRK-HKNJASU(IDX1)
           END-IF
      *
           PERFORM  210-HKN-SYUKEI-SEC
      *
      *    集計の対象にならなかったとき
           IF      FLG-UPD             =   1
               PERFORM 230-UPD-FILE-HENSYU-SEC
      *
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "集計対象外　患者番号＝"     DELIMITED  BY  SIZE
                       RECE10-PTNUM                DELIMITED  BY  SPACE
                       " 診療年月＝"               DELIMITED  BY  SIZE
                       RECE10-SRYYM                DELIMITED  BY  SIZE
                       " レセ種別＝"               DELIMITED  BY  SIZE
                       RECE10-RECESYUBETU          DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           END-IF 
           .
       2001-KOKUHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・老人保健分集計処理
      *****************************************************************
       210-HKN-SYUKEI-SEC          SECTION.
      *
      *    入外区分
           IF  RECE10-NYUGAIKBN  =  1
             MOVE  1  TO  IDX2
           ELSE
             MOVE  2  TO  IDX2
           END-IF
      *
      *    県内・県外区分
           IF  RECE10-PREFKBN  =  1
           OR  RECE10-HKNJANUM  =  ZERO
             MOVE  1  TO  IDX3
           ELSE
             MOVE  2  TO  IDX3
           END-IF
      *
      *    一般／退職者
             IF  RECE10-HKNNUM  =  "060" OR "068"
      *    年齢チェック
                 IF  RECE10-AGE  >=  70
                   EVALUATE  RECE10-KYURATE
                     WHEN  9       MOVE  1  TO  IDX1
                     WHEN  8
                       IF  RECE10-RECESYUBETU(4:1)  =  "7" OR "8"
                           MOVE    1    TO    IDX1
                       ELSE
                           MOVE    2    TO    IDX1
                       END-IF
                     WHEN  OTHER   MOVE  2  TO  IDX1
                   END-EVALUATE
                 ELSE
                  IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                  AND  (RECE10-FLG (1)   =  1)
                     MOVE  4     TO  IDX1
                  ELSE
                   IF  RECE10-AGE  >  2
                     MOVE  3  TO  IDX1
                   ELSE
                     MOVE  4  TO  IDX1
                   END-IF
                  END-IF
                 END-IF
             ELSE
               IF  RECE10-HONKZKKBN  =  "1"
                 MOVE   6        TO  IDX1
               ELSE
                IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                AND  (RECE10-FLG (1)   =  1)
                    MOVE  8      TO  IDX1
                ELSE
                 IF  RECE10-AGE  >  2
                   MOVE   7    TO  IDX1
                 ELSE
                   MOVE   8    TO  IDX1
                 END-IF
                END-IF
               END-IF
             END-IF
      *
      *    在総診・在医総か
           DISPLAY "RECE10-ZAITAKU***"RECE10-ZAITAKU
           IF  RECE10-ZAITAKU  NOT = SPACE
             PERFORM  960-SRYACT-SET-SEC
             IF  FLG-ZAIISO NOT = ZERO
      *在医総 １、一般在宅 ３、退職在宅 ５、老人在宅
               IF  IDX1  <  6
                 MOVE    1     TO  WRK-CNT
               ELSE
                 MOVE    3     TO  WRK-CNT
               END-IF
             END-IF
             IF  FLG-ZAISOSIN NOT = ZERO
      *在総診 ２、一般在宅 ４、退職在宅
               IF  IDX1  <  6
                 MOVE    2     TO  WRK-CNT
               ELSE
                 MOVE    4     TO  WRK-CNT
               END-IF
             END-IF
      **在医総 １、一般在宅 ３、退職在宅 ５、老人在宅
             PERFORM  2103-ZAI-SEC
           ELSE
             PERFORM  2102-SYUKEI-SEC
           END-IF
      *
      *    総合計
           ADD   1                  TO  SKT0300TMP-GKENSU(3 IDX3)
           ADD  RECE10-JNISSU (1)   TO  SKT0300TMP-GNISSU(3 IDX3)
           ADD  RECE10-TOTALTEN(1)  TO  SKT0300TMP-GTENSU(3 IDX3)
      *
      *    請求先処理（カウントしてない保険者のみ）
           IF  FLG-HKNJACNT  =  ZERO
             IF  RECE10-PREFKBN  =  1
             OR  RECE10-HKNJANUM  =  ZERO
               PERFORM  VARYING  IDX4  FROM  1  BY  1
                        UNTIL    HKNJA03-NUM(IDX4) = ZERO
                 IF  WRK-HKNJANUM  =  HKNJA03-HKNJANUM(IDX4)
                   MOVE  1  TO  SKT0300TMP-KENNAI-FLG(IDX4)
                   ADD   1  TO  SKT0300TMP-GOKEIHKNJA(1)
                   MOVE  40  TO  IDX4
                 END-IF
               END-PERFORM
             ELSE
               EVALUATE  WRK-HKNJANUM (1:2)
                 WHEN  "02"   ADD  1  TO  SKT0300TMP-KENGAI-KENSU (1)
                 WHEN  "04"   ADD  1  TO  SKT0300TMP-KENGAI-KENSU (2)
                 WHEN  "05"   ADD  1  TO  SKT0300TMP-KENGAI-KENSU (3)
                 WHEN  "06"   ADD  1  TO  SKT0300TMP-KENGAI-KENSU (4)
                 WHEN  "07"   ADD  1  TO  SKT0300TMP-KENGAI-KENSU (5)
                 WHEN  "09"   ADD  1  TO  SKT0300TMP-KENGAI-KENSU (6)
                 WHEN  "12"   ADD  1  TO  SKT0300TMP-KENGAI-KENSU (7)
                 WHEN  "13"   ADD  1  TO  SKT0300TMP-KENGAI-KENSU (8)
                 WHEN  "23"   ADD  1  TO  SKT0300TMP-KENGAI-KENSU (9)
                 WHEN  OTHER
                   PERFORM  VARYING  IDX4  FROM  1  BY  1
                            UNTIL    IDX4  >     50
                            OR       SKT0300TMP-KENMEI(IDX4) = ZERO
                     IF  WRK-HKNJANUM(1:2) = SKT0300TMP-KENMEI(IDX4)
                       MOVE  IDX4  TO  CNT-KEN
                       MOVE  50     TO  IDX4
                     END-IF
                   END-PERFORM
      *
      *    該当県名がなかった場合
                   IF  IDX4  <  50
                     MOVE  IDX4  TO  CNT-KEN
                     MOVE  WRK-HKNJANUM(1:2)
                                     TO  SKT0300TMP-KENMEI(CNT-KEN)
                   END-IF
      *
                   ADD  1  TO  SKT0300TMP-KENGAI-KENSU2 (CNT-KEN)
               END-EVALUATE
      *    合計
               ADD   1  TO  SKT0300TMP-GOKEIHKNJA(2)
             END-IF
           END-IF
      *
           IF  FLG-SKT0300TMP  =  1
             WRITE    SKT0300TMP-REC
           ELSE
             REWRITE  SKT0300TMP-REC
           END-IF
      *
           MOVE    1    TO  FLG-UPD
      *
           .
       210-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・集計処理
      *****************************************************************
       2102-SYUKEI-SEC         SECTION.
      *
      *    療養の給付：件数
           ADD  1                  TO  SKT0300TMP-KENSU(IDX1 IDX2 IDX3)
      *    療養の給付：診療実日数
           ADD  RECE10-JNISSU (1)  TO  SKT0300TMP-NISSU(IDX1 IDX2 IDX3)
      *    療養の給付：点数
           ADD  RECE10-TOTALTEN(1) TO  SKT0300TMP-TENSU(IDX1 IDX2 IDX3)
      *
      *    合計
           IF  IDX1  <  6
             MOVE    1              TO  IDX5
           ELSE
             MOVE    2              TO  IDX5
           END-IF
      *
           ADD   1                  TO  SKT0300TMP-GKENSU(IDX5 IDX3)
           ADD  RECE10-JNISSU (1)   TO  SKT0300TMP-GNISSU(IDX5 IDX3)
           ADD  RECE10-TOTALTEN(1)  TO  SKT0300TMP-GTENSU(IDX5 IDX3)
      *
           MOVE    1               TO  FLG-UPD
           .
       2102-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・在宅処理
      *****************************************************************
       2103-ZAI-SEC         SECTION.
      *
      *    療養の給付：件数
           ADD  1                  TO  SKT0300TMP-ZKENSU(WRK-CNT IDX3)
      *    療養の給付：診療実日数
           ADD  RECE10-JNISSU (1)  TO  SKT0300TMP-ZNISSU(WRK-CNT IDX3)
      *    療養の給付：点数
           ADD  RECE10-TOTALTEN(1) TO  SKT0300TMP-ZTENSU(WRK-CNT IDX3)
           DISPLAY "WRK-CNT****"WRK-CNT
           DISPLAY "IDX3   ****"IDX3
           DISPLAY "SKT0300TMP-ZTENSU***"SKT0300TMP-ZTENSU(WRK-CNT IDX3)
      *
      *    合計
           IF  WRK-CNT  =  2
               MOVE        1       TO  WRK-CNT
           END-IF
      *
           IF  WRK-CNT  =  3   OR  4
               MOVE        2       TO  WRK-CNT
           END-IF
      *
           ADD  1                  TO  SKT0300TMP-GKENSU(WRK-CNT IDX3)
           ADD  RECE10-JNISSU (1)  TO  SKT0300TMP-GNISSU(WRK-CNT IDX3)
           ADD  RECE10-TOTALTEN(1) TO  SKT0300TMP-GTENSU(WRK-CNT IDX3)
      *
           .
       2103-ZAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・食事療養
      *****************************************************************
      * 2104-SHOKUJI-SEC         SECTION.
      **
      **    食事療養：件数
      *     ADD  1                   TO  SKT0300TMP-SKENSU(WRK-CNT IDX3)
      **    食事療養：日数
      *     ADD  RECE10-SHOKUJINISSU (1)
      *                              TO  SKT0300TMP-SNISSU(WRK-CNT IDX3)
      **    食事療養：食事療養費
      *     ADD  RECE10-SHOKUJIRYOYOHI (1)
      *                              TO  SKT0300TMP-STENSU(WRK-CNT IDX3)
      **
      **    食事療養合計
      *     ADD  1                   TO  SKT0300TMP-GKENSU(5 IDX3)
      *     ADD  RECE10-SHOKUJINISSU(1)
      *                              TO  SKT0300TMP-GNISSU(5 IDX3)
      *     ADD  RECE10-SHOKUJIRYOYOHI(1)
      *                              TO  SKT0300TMP-GTENSU(5 IDX3)
      *     DISPLAY "IDX3=" IDX3
      *     DISPLAY "WRK-CNT=" WRK-CNT
      *     DISPLAY "SKT0300TMP-SKENSU(WRK-CNT IDX3)"
      *                     SKT0300TMP-SKENSU(WRK-CNT IDX3)
      **
      *     .
      * 2104-SHOKUJI-EXT.
      *     EXIT.
      *=== 帳票様式の変更 by Yoshikawa(2012/04/19) end
057400*
      *****************************************************************
006900*    請求ＤＢ更新用ファイル出力処理
      *****************************************************************
       230-UPD-FILE-HENSYU-SEC         SECTION.
      *
           MOVE    RECE10-REC          TO  RECE10X-REC
           WRITE   RECE10X-REC
           ADD     1                   TO  CNT-UPDFILE
           .
       230-UPD-FILE-HENSYU-EXT.
           EXIT.
057400*
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           OPEN     INPUT       SKT0300TMP-FILE
      *
           MOVE     1           TO       WRK-CNT
           MOVE     ZERO        TO       FLG-PASS
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =  1
      *
             PERFORM  UNTIL       SKT0300TMP-KENMEI (WRK-CNT) = ZERO
               AND    FLG-PASS    =        1
      *
      *    帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
      *    帳票印刷処理
               PERFORM 330-PRINT-OUT-SEC
               MOVE    1          TO       FLG-PASS
             END-PERFORM
             MOVE  ZERO   TO  FLG-PASS
      *      一時ファイル読込
             PERFORM  900-TMP-READ-SEC
             MOVE  1  TO  WRK-CNT
           END-PERFORM
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT0300
      *
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT0300-SRYYM
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT0300-ADRS
           INSPECT SKT0300-ADRS          REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT0300-HOSPNAME
           INSPECT SKT0300-HOSPNAME      REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE    WRK-KAISETUNAME       TO  SKT0300-KAISETUNAME
      *
      *    電話番号
           MOVE    WRK-HOSPTEL           TO  SKT0300-TEL
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN      TO  SKT0300-HOSPCDN
      *
      *    組合負担割合
           MOVE    SPACE                 TO  SKT0300-KYURATE
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT0300-TOKUBETU
           END-IF
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
      *    編集処理
           IF     WRK-CNT          =     1
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     8
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     2
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                          UNTIL    IDX3  >     2
      *    療養の給付：件数
                   MOVE  SKT0300TMP-KENSU (IDX1 IDX2 IDX3)
                                                  TO  WRK-Z5
                   MOVE  WRK-Z5   TO  SKT0300-KENSU(IDX1 IDX2 IDX3)
      *    療養の給付：日数
                   MOVE  SKT0300TMP-NISSU (IDX1 IDX2 IDX3)  
                                                  TO  WRK-Z5
                   MOVE  WRK-Z5   TO  SKT0300-NISSU(IDX1 IDX2 IDX3)
      *    療養の給付：点数
                   MOVE  SKT0300TMP-TENSU (IDX1 IDX2 IDX3) 
                                                  TO  WRK-Z9
                   MOVE  WRK-Z9   TO  SKT0300-TENSU(IDX1 IDX2 IDX3)
                 END-PERFORM
               END-PERFORM
             END-PERFORM
      *
      *    在宅処理
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     4
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     2
      *    療養の給付：件数
                   MOVE  SKT0300TMP-ZKENSU (IDX1 IDX2)   TO  WRK-Z5
                   MOVE  WRK-Z5   TO  SKT0300-ZKENSU(IDX1 IDX2)
      *    療養の給付：日数
                   MOVE  SKT0300TMP-ZNISSU (IDX1 IDX2)   TO  WRK-Z5
                   MOVE  WRK-Z5   TO  SKT0300-ZNISSU(IDX1 IDX2)
      *    療養の給付：点数
                   MOVE  SKT0300TMP-ZTENSU (IDX1 IDX2)   TO  WRK-Z9
                   MOVE  WRK-Z9   TO  SKT0300-ZTENSU(IDX1 IDX2)
               END-PERFORM
             END-PERFORM
      *
      *    合計処理
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     3
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     2
      *    療養の給付：件数
                 MOVE  SKT0300TMP-GKENSU (IDX1 IDX2)   TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT0300-GKENSU(IDX1 IDX2)
      *    療養の給付：日数
                 MOVE  SKT0300TMP-GNISSU (IDX1 IDX2)   TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT0300-GNISSU(IDX1 IDX2)
      *    療養の給付：点数
                 MOVE  SKT0300TMP-GTENSU (IDX1 IDX2)   TO  WRK-Z9
                 MOVE  WRK-Z9   TO  SKT0300-GTENSU(IDX1 IDX2)
               END-PERFORM
             END-PERFORM
      *
      *    請求先名（県内）
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     40
               IF  SKT0300TMP-KENNAI-FLG (IDX1)  =  1
                 MOVE  "○"       TO  SKT0300-NHKNJA (IDX1)
               END-IF
             END-PERFORM
      *
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     2
               MOVE  SKT0300TMP-GOKEIHKNJA (IDX1)  TO  WRK-Z3
               MOVE  WRK-Z3     TO       SKT0300-GOKEIHKNJA(IDX1)
             END-PERFORM
      *
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     9
               MOVE  SKT0300TMP-KENGAI-KENSU (IDX1)  TO  WRK-Z2
               MOVE  WRK-Z2               TO  SKT0300-GHKNJA (IDX1)
             END-PERFORM
      *
           END-IF
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                   UNTIL    IDX1  >     5
                    OR       SKT0300TMP-KENMEI (WRK-CNT) = ZERO
             EVALUATE  SKT0300TMP-KENMEI (WRK-CNT)
               WHEN  "01"  MOVE  "北海道"  TO  SKT0300-KENMEI (IDX1)
               WHEN  "08"  MOVE  "茨城"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "10"  MOVE  "群馬"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "11"  MOVE  "埼玉"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "14"  MOVE  "神奈川"  TO  SKT0300-KENMEI (IDX1)
               WHEN  "15"  MOVE  "新潟"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "16"  MOVE  "富山"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "17"  MOVE  "石川"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "18"  MOVE  "福井"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "19"  MOVE  "山梨"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "20"  MOVE  "長野"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "21"  MOVE  "岐阜"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "22"  MOVE  "静岡"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "24"  MOVE  "三重"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "25"  MOVE  "滋賀"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "26"  MOVE  "京都"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "27"  MOVE  "大阪"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "28"  MOVE  "兵庫"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "29"  MOVE  "奈良"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "30"  MOVE  "和歌山"  TO  SKT0300-KENMEI (IDX1)
               WHEN  "31"  MOVE  "鳥取"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "32"  MOVE  "島根"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "33"  MOVE  "岡山"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "34"  MOVE  "広島"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "35"  MOVE  "山口"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "36"  MOVE  "徳島"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "37"  MOVE  "香川"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "38"  MOVE  "愛媛"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "39"  MOVE  "高知"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "40"  MOVE  "福岡"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "41"  MOVE  "佐賀"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "42"  MOVE  "長崎"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "43"  MOVE  "熊本"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "44"  MOVE  "大分"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "45"  MOVE  "宮崎"    TO  SKT0300-KENMEI (IDX1)
               WHEN  "46"  MOVE  "鹿児島"  TO  SKT0300-KENMEI (IDX1)
               WHEN  "47"  MOVE  "沖縄"    TO  SKT0300-KENMEI (IDX1)
             END-EVALUATE
             MOVE  SKT0300TMP-KENGAI-KENSU2(WRK-CNT)  TO  WRK-Z2
             MOVE  WRK-Z2                  TO  SKT0300-GHKNJA2 (IDX1)
             ADD   1                       TO  WRK-CNT
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       330-PRINT-OUT-SEC                SECTION.
      *
           DISPLAY  "PRINT"
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "SKT0300.red"           TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "国保総括表"      TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "国保総括表［返戻分］" 
                                       TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "国保総括表［特別療養費］"
                                       TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "国保総括表［返戻分］［特別療養費］"
                                       TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT0300             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "2"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       330-PRINT-OUT-EXT.
           EXIT.
057400*
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *   請求管理ＤＢ更新処理
      *****************************************************************
       310-SEIKYU-UPD-SEC              SECTION.
      *
           OPEN    INPUT    UPD-FILE
      *
           PERFORM 900-UPD-FILE-READ-SEC
           PERFORM             UNTIL   FLG-UPDFILE    =   1
               MOVE    RECE10X-KEY         TO  RECE10-KEY
               MOVE    RECE10-REC          TO  MCPDATA-REC
               PERFORM 910-SEIKYU-INV-SEC
               IF      FLG-SEIKYU          =   ZERO
                   MOVE    "2"             TO  RECE10-SKYKBN
      *
                   MOVE    LNK-PRTKANRI-SRYYM
                                           TO  RECE10-SKYYM
                   MOVE    LNK-PRTKANRI-SKYYMD
                                           TO  RECE10-UPDYMD
      *
                   MOVE    RECE10-REC            TO  MCPDATA-REC
                   MOVE    "DBUPDATE"            TO  MCP-FUNC
                   MOVE    "tbl_seikyu"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"     USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
                   IF      MCP-RC          =   ZERO
                       PERFORM 900-UPD-FILE-READ-SEC
                   ELSE
                       DISPLAY "UPD ERR KEY="  RECE10-KEY 
                       MOVE  "*** SOKATU0300 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                   MOVE  "*** SOKATU0300 SEIKYU INVALID ***"
                                           TO  WRK-RECEERR
                   MOVE    RECE10-KEY      TO  WRK-RECEERR(33:)
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1               TO  FLG-UPDFILE
               END-IF
           END-PERFORM  
      *
           CLOSE   UPD-FILE
           .
       310-SEIKYU-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *  診療情報セット
      *****************************************************************
       960-SRYACT-SET-SEC  SECTION.
      *
             INITIALIZE  SRYACT-REC
             INITIALIZE  FLG-ZAIISO
             INITIALIZE  FLG-ZAISOSIN
             MOVE  SPA-HOSPNUM             TO  SRY-HOSPNUM
             MOVE  RECE10-NYUGAIKBN        TO  SRY-NYUGAIKBN
             MOVE  RECE10-PTID             TO  SRY-PTID
             MOVE  RECE10-SRYYM            TO  SRY-SRYYM
             MOVE  SRYACT-REC              TO  MCPDATA-REC
             MOVE  "DBSELECT"              TO  MCP-FUNC
             MOVE  "tbl_sryact"            TO  MCP-TABLE
             MOVE  "key4"                  TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_sryact"          TO  MCP-TABLE
               MOVE  "key4"                TO  MCP-PATHNAME
               PERFORM  965-SRYACT-READ-SEC
      *    在宅フラグ所持者から在総診の患者取得
               PERFORM  UNTIL  FLG-SRYACT  =  1
                 PERFORM  965-ZAISOU-CHK-SEC
      *
                 MOVE  "tbl_sryact"        TO  MCP-TABLE
                 MOVE  "key4"              TO  MCP-PATHNAME
                 PERFORM  965-SRYACT-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SRYACT SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
             MOVE  "tbl_sryact"            TO  MCP-TABLE
             MOVE  "key4"                  TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       960-SRYACT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療情報読込
      *****************************************************************
       965-SRYACT-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL  "ORCDBMAIN"    USING
                                MCPAREA
                                MCPDATA-REC
                                SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  SRYACT-REC
             MOVE  ZERO         TO  FLG-SRYACT
           ELSE
             INITIALIZE  SRYACT-REC
             MOVE  1  TO  FLG-SRYACT
           END-IF
      *
           .
       965-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    在総診チェック
      *****************************************************************
       965-ZAISOU-CHK-SEC  SECTION.
      *
           PERFORM  VARYING  IDX4  FROM  1  BY  1
                    UNTIL   (IDX4  >     5) OR
                            (SRY-SRYCD(IDX4)  =  SPACE)
             IF  SRY-SRYKBN  =  "14"
               IF  RECE10-SRYYM  >=  WRK-CONS-SRYYM-201604
      *          在総診判定
                 PERFORM  VARYING  IDX5  FROM  1  BY  1
                          UNTIL   (IDX5  >  TBL-ZAISOSIN2-MAX)
                          OR      (FLG-ZAISOSIN  =  1)
                   IF  SRY-SRYCD(IDX4)  =  TBL-ZAISOSIN2(IDX5)
                       MOVE    1            TO  FLG-ZAISOSIN
                   END-IF
                 END-PERFORM
      *          現状、在総診以外（特医総など）は全部在医総とする
                 IF  FLG-ZAISOSIN  =  ZERO
                     MOVE    1              TO  FLG-ZAIISO
                 END-IF
      *          在医総だけ集計するのであれば上記をコメント化し
      *          以下のコメントを外して下さい
      *          在管医総判定
      *           PERFORM  VARYING  IDX5  FROM  1  BY  1
      *                    UNTIL   (IDX5  >  TBL-ZAIISO2-MAX)
      *                    OR      (FLG-ZAIISO  =  1)
      *             IF  SRY-SRYCD(IDX4)  =  TBL-ZAIISO2(IDX5)
      *                 MOVE    1            TO  FLG-ZAIISO
      *             END-IF
      *           END-PERFORM
               ELSE
                IF  RECE10-SRYYM  >=  WRK-CONS-SRYYM-201404
      *           在総診判定
                  PERFORM  VARYING  IDX5  FROM  1  BY  1
                           UNTIL   (IDX5  >  TBL-ZAISOSIN-MAX)
                           OR      (FLG-ZAISOSIN  =  1)
                    IF  SRY-SRYCD(IDX4)  =  TBL-ZAISOSIN(IDX5)
                        MOVE    1            TO  FLG-ZAISOSIN
                    END-IF
                  END-PERFORM
                  IF  FLG-ZAISOSIN  =  ZERO
                      MOVE    1              TO  FLG-ZAIISO
                  END-IF
                ELSE
                  IF  SRY-SRYCD(IDX4)  =  CONS-ZAITAKU1  OR
                                          CONS-ZAITAKU2
                    MOVE    1      TO  FLG-ZAISOSIN
                  ELSE
                    MOVE    1      TO  FLG-ZAIISO
                  END-IF
                END-IF
               END-IF
             END-IF
           END-PERFORM
             DISPLAY "**FLG-ZAIISO***"FLG-ZAIISO
             DISPLAY "**FLG-ZAISOSIN***"FLG-ZAISOSIN
      *
           .
       965-ZAISOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
      *
           MOVE   SPA-HOSPNUM           TO  RECE10-HOSPNUM
           MOVE   LNK-PRTKANRI-SRYYM    TO  RECE10-SKYYM.
           MOVE   SPACE                 TO  RECE10-XXSTHKNJANUM
           MOVE   "99999999"            TO  RECE10-XXEDHKNJANUM
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  000000               TO  RECE10-SRYYM
             MOVE  999999               TO  RECE10-XXSRYYM
           END-IF
           MOVE   RECE10-REC            TO  MCPDATA-REC
           MOVE   "DBSELECT"            TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    WRK-PATH             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    WRK-PATH                TO  MCP-PATHNAME
           MOVE    "DBFETCH"               TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE10-REC
      *
               IF      WRK-PARA-STSRYYM    =   "999999"
                   CONTINUE
               ELSE
                   IF  ( RECE10-SRYYM      <   WRK-PARA-STSRYYM )
                   OR  ( RECE10-SRYYM      >   WRK-PARA-EDSRYYM )
                       GO  TO  900-SEIKYU-READ-SEC
                   END-IF
               END-IF
      *
      *        返戻分でオンライン請求は読み飛ばす
               IF      (WRK-PARA-SYOKBN         =  "2")
               AND     (RECE10-HENREI-SIJI-FLG  =  1)
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *
      *        テスト患者のときは読み飛ばす
               MOVE    SPACE               TO  PTINF-REC
               INITIALIZE                      PTINF-REC
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    RECE10-PTID         TO  PTINF-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 800-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
               AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF   
      *         
               ADD     1                   TO  CNT-RECE10   
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"             TO  MCP-FUNC
           MOVE    "tbl_ptinf"            TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"              TO  MCP-FUNC
               MOVE    "tbl_ptinf"        TO  MCP-TABLE
               MOVE    "key"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"        USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計ファイル読込
      *****************************************************************
       800-SKTTMP-READ-SEC             SECTION.
      *
           READ    SKT0300TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT0300TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT0300TMP
           END-READ
      *
           .
       800-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理ＤＢ更新用ファイル読込
      *****************************************************************
       900-UPD-FILE-READ-SEC             SECTION.
      *
           READ    UPD-FILE
               AT  END
                   MOVE    1           TO  FLG-UPDFILE
               NOT AT  END
                   MOVE    ZERO        TO  FLG-UPDFILE
           END-READ
           .
       900-UPD-FILE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_syskanri"             TO  MCP-TABLE
           MOVE    "key10"                    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                   TO  MCP-FUNC
               MOVE    "tbl_syskanri"         TO  MCP-TABLE
               MOVE    "key10"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"            USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
           MOVE    "tbl_syskanri"         TO  MCP-TABLE
           MOVE    "key10"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタ読込
      *****************************************************************
       910-SEIKYU-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"              TO  MCP-FUNC
               MOVE    "tbl_seikyu"         TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SEIKYU
                   MOVE    MCPDATA-REC         TO  RECE10-REC
               ELSE
                   MOVE    1                   TO  FLG-SEIKYU
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       910-SEIKYU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  SKT0300TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者データ読込
      *****************************************************************
       110-TOWNINF-GET-SEC  SECTION.
      *
      *    ファイルディレクトリ位置指定
           INITIALIZE           ORCSMKPASSAREA
           MOVE  DATA-FILE     TO  MKPASS-IN-01
           CALL  "ORCSMKPASSSITE"  USING  ORCSMKPASSAREA
           MOVE  MKPASS-OT-01  TO  FILE-NAME
      *
           OPEN  INPUT  HKNJA-FILE
           INITIALIZE  HKNJA03-DAT
      *
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    FLG-TOWN = 1
             READ HKNJA-FILE NEXT
               AT END
                 MOVE  1         TO  FLG-TOWN
               NOT AT END
                 MOVE  HKNJA-REC  TO  HKNJA03-TBL(IDX1)
             END-READ
           END-PERFORM
      *
           CLOSE HKNJA-FILE
      *
           .
       110-TOWNINF-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE                   TO  SYS-1900-REC
           INITIALIZE                          SYS-1900-REC
           MOVE    "1900"                  TO  SYS-1900-KANRICD
           MOVE    "*"                     TO  SYS-1900-KBNCD
           MOVE    SPA-HOSPNUM             TO  SYS-1900-HOSPNUM
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1900-STYUKYMD(1:6)
                                               SYS-1900-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1900-STYUKYMD(1:6)
                                               SYS-1900-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                    TO  SYS-1900-STYUKYMD(7:2)
                                               SYS-1900-EDYUKYMD(7:2)
           MOVE    SYS-1900-REC            TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(8) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
      *            医療機関住所編集情報（２）
                   PERFORM 900-1903-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
           .
      *
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE               TO  SYS-1901-REC
           INITIALIZE                      SYS-1901-REC
           MOVE    "1901"              TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(8)  TO  SYS-1901-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1901-HOSPNUM
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-STYUKYMD(1:6)
                                               SYS-1901-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1901-STYUKYMD(1:6)
                                               SYS-1901-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1901-STYUKYMD(7:2)
                                           SYS-1901-EDYUKYMD(7:2)
           MOVE    SYS-1901-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSPNAME1
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-HOSPNAME2
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
           .
      *
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE               TO  SYS-1902-REC
           INITIALIZE                      SYS-1902-REC
           MOVE    "1902"              TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(8)  TO  SYS-1902-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1902-HOSPNUM
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD(1:6)
                                               SYS-1902-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1902-STYUKYMD(1:6)
                                               SYS-1902-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1902-STYUKYMD(7:2)
                                           SYS-1902-EDYUKYMD(7:2)
           MOVE    SYS-1902-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
           .
      *
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1903-READ-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1903-REC
           INITIALIZE                      SYS-1903-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1903-HOSPNUM
           MOVE    "1903"              TO  SYS-1903-KANRICD
           MOVE    SYS-1900-PRTKBN(8)  TO  SYS-1903-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1903-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1903-STYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1903-STYUKYMD(7:2)
           MOVE    SYS-1903-STYUKYMD   TO  SYS-1903-EDYUKYMD
           MOVE    SYS-1903-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1903-REC
               MOVE    SYS-1903-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           ELSE
               INITIALIZE                  SYS-1903-REC
           END-IF
           .
      *
       900-1903-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .                             
       900-DBCLOSE-EXT.
           EXIT.
