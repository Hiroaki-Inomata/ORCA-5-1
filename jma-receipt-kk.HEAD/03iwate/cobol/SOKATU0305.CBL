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
       PROGRAM-ID.             SOKATU0305.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 岩手・広域連合・診療報酬請求総括表
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/04/28    吉川          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2008/05/15  在医総と在総診の印字位置が
      *                                  逆になっているのを修正
      *                                  請求冊数カウントミス修正
      *  01.00.02    吉川    2008/10/22  在総診以外の在宅は在医総とする
      *  01.00.03    吉川    2010/02/17  広域連合の保険者番号指定対応
      *  01.00.04    吉川    2010/07/01  オンライン返戻対応
      *  01.00.05    吉川    2010/09/22  日数の桁数増加
      *  01.00.06    吉川    2011/04/11  東北地方太平洋沖地震対応
      *  01.00.07    吉川    2012/04/19  帳票様式の変更に伴う修正
      *                                  シス管「1901」で設定した電話番号が
      *                                  印字されるように修正
      *                                  PERFORM→GOTOに修正
      *  01.00.08    吉川    2012/12/19  医療機関情報編集修正
      *  01.00.09    吉川    2014/05/20  在宅の対象とする診療コード変更
      *  01.00.10    吉川    2014/10/03  ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.11    吉川    2016/04/14  医療機関編集情報への開設者名追加対応
      *  01.00.12    吉川    2016/05/16  在宅コード変更（平成２８年４月改正）
      *  01.00.13    門間    2017/01/11  医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *  01.00.14    萩野    2017/03/08  医療機関情報の取得の修正
      *  01.00.15    門間    2018/04/09  SITEKBN変更（動作に変更なし）
      *  01.00.16    井上    2018/10/10  修正履歴コメント削除
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    岩手保険者データ
           SELECT  HKNJA-FILE     ASSIGN FILE-NAME
                                 ORGANIZATION   IS  LINE SEQUENTIAL
                                 ACCESS MODE    IS  SEQUENTIAL
                                 FILE   STATUS  IS  STS-TOWN.
      *
      *    請求ＤＢ更新用ファイル
           SELECT  UPD-FILE        ASSIGN  RECEUPD
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-RECEUPD.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
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
      *    請求ＤＢ更新用ファイル
       FD  UPD-FILE.
           COPY    "CPRCF010.INC"  REPLACING  //RECE10//
                                   BY         //RECE10X//.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    請求ＤＢ更新用ファイル 名称領域 
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
           03  FILLER       PIC X(13)  VALUE  "HKNJA0305.TXT".
      *
      *    岩手保険者データ
           COPY  "HKNJA0300.INC".
      *
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
           COPY    "SKT0305.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-TOWN            PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-KOH-END         PIC 9(01).
           03  FLG-UPD             PIC 9(01).
      *     
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01). 
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01). 
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-ZAIISO          PIC 9(01).
           03  FLG-ZAIISO2         PIC 9(01).
           03  FLG-TOWN            PIC 9(01).
           03  FLG-HKNJACNT        PIC 9(01).
           03  FLG-PASS            PIC 9(01).
           03  FLG-ZAISOSIN        PIC 9(01).
      *     
           03  FLG-PRTKBN          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDXY                PIC 9(04).
      *
           03  IDY                 PIC 9(04).
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDY3                PIC 9(04).
           03  IDY4                PIC 9(04).
      *     
           03  IDZ                 PIC 9(04).
           03  IDZ1                PIC 9(04).
           03  IDZ2                PIC 9(04).
      *
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HKNJANUM   PIC X(08).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-SYOKBN1    PIC X(01).
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
           03  WRK-KOHNUM          PIC X(03).
           03  WRK-HKNJANUM        PIC X(06).
           03  WRK-HKNJANAME       PIC X(20).
           03  WRK-HKNJASU-TBL  OCCURS  300.
               05  WRK-HKNJASU     PIC 9(06).
      *
           03  WRK-Z2              PIC ZZ.
           03  WRK-Z3              PIC ZZZ.
           03  WRK-Z4              PIC ZZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z6              PIC ZZ,ZZZ.
           03  WRK-Z7              PIC ZZZ,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-KOHNUM9         PIC Z9.
           03  WRK-PAGE            PIC ZZZ9.
      *
           03  WRK-MAE-INPUT       PIC X(200).
           03  WRK-OUT-INPUT       PIC X(200).
      *
           03  WRK-RECEERR         PIC X(200). 
           03  WRK-ERRMSG          PIC X(300). 
      *
           03  WRK-PATH            PIC X(50).
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-SYOKBN          PIC X(01).
           03  WRK-INDEX           PIC 9(02).
      *
      *    公費名称編集
           03  WRK-KOHKBN.
               05  WRK-KOHNUM        PIC X(03).
               05  WRK-HKNNAME       PIC X(20). 
           03  WRK-MEISYO          PIC X(44).
      *
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
       01  WRK-2007-AREA.
           03  WRK-2007-TBL        OCCURS  100.
               05  WRK-2007-MEISYO PIC X(44).
               05  WRK-2007-KOH-AREA.
                   07  WRK-2007-KOHNUM
                                   PIC X(03)   OCCURS  10.
           03  WRK-2007-MAX        PIC 9(03).
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
           03  CNT-KOH             PIC 9(02).
           03  CNT-KEN             PIC 9(02).
      *
      *    集計領域
       01  SUM-AREA.
           03  SUM-TBL-SRYKA       PIC X(02).
           03  SUM-TABLE.
      *        請求
               05  SUM-SEIKYU-TBL   OCCURS  2.
                   07  SUM-NYUGAI-TBL  OCCURS  2.
                       09  SUM-PREF-TBL  OCCURS  2.
                           11  SUM-KENSU       PIC  9(06).
                           11  SUM-NISSU       PIC  9(06).
                           11  SUM-TENSU       PIC  9(10).
      *        在宅
               05  SUM-ZAI-TBL      OCCURS  2.
                   07  SUM-ZPREF-TBL  OCCURS  2.
                       09  SUM-ZKENSU          PIC  9(06).
                       09  SUM-ZNISSU          PIC  9(06).
                       09  SUM-ZTENSU          PIC  9(10).
      *        食事療養費
               05  SUM-SKJ-TBL.
                   07  SUM-SPREF-TBL  OCCURS  2.
                       09  SUM-SKENSU          PIC  9(06).
                       09  SUM-SNISSU          PIC  9(06).
                       09  SUM-STENSU          PIC  9(10).
      *        合計
               05  SUM-GOKEI-TBL    OCCURS  2.
                   07  SUM-GPREF-TBL  OCCURS  2.
                       09  SUM-GKENSU          PIC  9(06).
                       09  SUM-GNISSU          PIC  9(06).
                       09  SUM-GTENSU          PIC  9(10).
      *        給付割合
               05  SUM-KYURATE                 PIC  9(02).
      *        県内保険者
               05  SUM-NHKNJA-TBL   OCCURS  36.
                   07  SUM-KENNAI-FLG          PIC  9(01).
      *        県名
               05  SUM-KENMEI-TBL   OCCURS  50.
                   07  SUM-KENMEI              PIC  9(02).
      *        県外保険者
               05  SUM-GHKNJA-TBL   OCCURS  9.
                   07  SUM-KENGAI-KENSU        PIC  9(02).
      *        県外保険者２
               05  SUM-GHKNJA-TBL2  OCCURS  50.
                   07  SUM-KENGAI-KENSU2       PIC  9(02).
      *        合計保険者
               05  SUM-GOKEIHKNJA-TBL  OCCURS  2.
                   07  SUM-GOKEIHKNJA          PIC  9(03).

      *
      *    キー領域
       01  KEY-AREA                    VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-HKNJANUM      PIC X(08).
           03  KEY-OLD.
               05  KEY-O-HKNJANUM      PIC X(08). 
      *
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
           03  WRK-CONS-SRYYM-201404   PIC X(06)   VALUE   "201404".
           03  WRK-CONS-SRYYM-201604   PIC X(06)   VALUE   "201604".
      *
      *在総診テーブル
       01  TBL-ZAITAKU-AREA.
      *    在宅総合・在宅末期
           03  CONS-ZAITAKU1           PIC X(09) VALUE "114007610".
           03  CONS-ZAITAKU2           PIC X(09) VALUE "114007710".
           03  CONS-ZAITAKU3           PIC X(09) VALUE "114007510".
           03  CONS-ZAITAKU4           PIC X(09) VALUE "114012210".
           03  CONS-ZAITAKU5           PIC X(09) VALUE "114012310".
           03  CONS-ZAITAKU6           PIC X(09) VALUE "114012410".
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
       01  TBL-ZAIISO-R  REDEFINES     TBL-ZAIISO-AREA.
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
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
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
      *    診療科目情報
           COPY    "CPSK1005.INC".
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
      *    レセプト・総括印刷情報（広域連合）
           COPY    "CPSK200501.INC".
      *
      *    公費番号まとめ情報
           COPY    "CPSK2007.INC".
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
      *   半角チェックサブ
           COPY  "CPORCSKANACHK.INC".
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
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
           DISPLAY  "START"
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    市町村情報
           PERFORM 110-TOWNINF-GET-SEC
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
                                               WRK-PARA-HKNJANUM
                                               WRK-PARA-SYOKBN
                                               WRK-PARA-SYOKBN1
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
           MOVE    "SOKATU0305"    TO  JOB-PGID
           MOVE    "広域連合・診療報酬請求書"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (2)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEERR
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
           PERFORM 910-SYSKANRI-KEY10-SEC
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
           PERFORM 910-SYSKANRI-KEY10-SEC
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
      *    レセプト・総括編集情報
      *    区分コード０１のデータ読み込み
           MOVE    SPACE               TO  SYS-200501-REC
           INITIALIZE                      SYS-200501-REC
           MOVE    "2005"              TO  SYS-200501-KANRICD
           MOVE    "01"                TO  SYS-200501-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-200501-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-200501-STYUKYMD (7:2)
           MOVE    SYS-200501-STYUKYMD TO  SYS-200501-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-200501-HOSPNUM
           MOVE    SYS-200501-REC      TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-200501-REC
           ELSE
               INITIALIZE                      SYS-200501-REC
           END-IF
           IF      SYS-200501-KOUIKIPRTKBN
                                       =   SPACE
               MOVE    "0"                 TO  SYS-200501-KOUIKIPRTKBN
           END-IF
      *
           IF      SYS-200501-KOUIKIPRTKBN
                                       =   "1"
               CONTINUE
           ELSE
               MOVE    "00000000"          TO  WRK-PARA-HKNJANUM
           END-IF
      *
           DISPLAY "SYS-200501-KOUIKIPRTKBN=" SYS-200501-KOUIKIPRTKBN
           DISPLAY "WRK-PARA-HKNJANUM      =" WRK-PARA-HKNJANUM
      *
           .
       100-INIT-EXT.
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
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    OUTPUT  UPD-FILE
      *
      *    処理区分(0=全体 1=当月・月遅れ 2=返戻分)で分ける
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    ZERO
               WHEN    "2"
                   MOVE    "2"         TO  WRK-SYOKBN
                   PERFORM 2001-SYOKBN-SYUKEI-SEC
           END-EVALUATE
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    ZERO
               WHEN    "1"
                   MOVE    "1"         TO  WRK-SYOKBN
                   PERFORM 2001-SYOKBN-SYUKEI-SEC
           END-EVALUATE
      *
           CLOSE   UPD-FILE
      *
      *    請求ＤＢ更新
           IF      FLG-UPD        =   1
           AND     FLG-ERR        =   ZERO 
               IF      CNT-UPDFILE    >   ZERO
                   PERFORM 310-SEIKYU-UPD-SEC
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理区分集計処理
      *****************************************************************
       2001-SYOKBN-SYUKEI-SEC         SECTION.
      *
      *    入院外（その他のレセ）・入院集計
      *    
           MOVE    LOW-VALUE           TO  KEY-AREA
      *
      *    処理区分によってキーを分ける(SYOKBN1=2→特別療養費)
           EVALUATE    WRK-SYOKBN      ALSO    WRK-PARA-SYOKBN1
               WHEN    "1"             ALSO    "1"
                   MOVE    "key66"         TO  WRK-MCP-PATHNAME
               WHEN    "2"             ALSO    "1"
                   MOVE    "key65"         TO  WRK-MCP-PATHNAME
               WHEN    "1"             ALSO    "2"
                   MOVE    "key66"         TO  WRK-MCP-PATHNAME
               WHEN    "2"             ALSO    "2"
                   MOVE    "key65"         TO  WRK-MCP-PATHNAME
           END-EVALUATE  
           MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
           PERFORM 900-SEIKYU-SELECT-SEC
      *
           PERFORM 200-HENSYU-SEC  UNTIL   FLG-RECE10   =   1 
                                   OR      FLG-ERR      =   1
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    1                   TO  FLG-END
      *
           .
       2001-SYOKBN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       200-HENSYU-SEC                SECTION.
      *
           INITIALIZE                  SUM-AREA
      *
           PERFORM             UNTIL   FLG-END     =   1
                               OR      FLG-RECE10  =   1
      *        保険者別集計処理
               PERFORM 2001-KOUKI-SYUKEI-SEC
           END-PERFORM
      *
      *   印刷処理
          IF      FLG-UPD        =   1
          AND     FLG-ERR        =   ZERO
               PERFORM 2002-PRINT-SEC
          END-IF
           .
       200-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者別集計処理
      *****************************************************************
       2001-KOUKI-SYUKEI-SEC                SECTION.
      *
           INITIALIZE                       IDX-AREA
                                            FLG-UPD
                                            FLG-HKNJACNT
                                            WRK-HKNJANUM
      *
      *    保険者番号
           IF  RECE10-HKNJANUM(7:2)  =  SPACE
             MOVE  RECE10-HKNJANUM(1:6)  TO  WRK-HKNJANUM(1:6)
           ELSE
             MOVE  RECE10-HKNJANUM(3:6)  TO  WRK-HKNJANUM(1:6)
           END-IF
      *
           IF  RECE10-ETC-YKZFTN(1)  NOT =  ZERO
           AND RECE10-HKNJANUM  =  ZERO
             MOVE  "999999"              TO  WRK-HKNJANUM
      *      保険者不明分は県内へ
             MOVE  "1"                   TO  RECE10-PREFKBN
           END-IF
      *
      *    保険者数（既にカウントした保険者か）
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
      *    給付割合
           EVALUATE    RECE10-KYURATE
               WHEN    9
                   MOVE    1                   TO  IDX1
               WHEN    7
                   MOVE    2                   TO  IDX1
           END-EVALUATE
      *
      *    入外区分
           EVALUATE    RECE10-NYUGAIKBN
               WHEN    "1"
                   MOVE    1                   TO  IDX2
               WHEN    "2"
                   MOVE    2                   TO  IDX2
           END-EVALUATE
      *
      *    県内・県外区分
           EVALUATE    RECE10-PREFKBN
               WHEN    1
                   MOVE    1                   TO  IDX3
               WHEN    2
                   MOVE    2                   TO  IDX3
           END-EVALUATE
      *
      *    在宅集計
           IF   RECE10-ZAITAKU  =  1
             PERFORM  960-SRYACT-SET-SEC
      *      在総診
             IF  FLG-ZAIISO  NOT = ZERO
                 MOVE      1                   TO  IDX1
             END-IF
      *      在医総
             IF  FLG-ZAISOSIN  NOT = ZERO
                 MOVE      2                   TO  IDX1
             END-IF
             PERFORM  20011-ZAI-SYUKEI-SEC
           ELSE
             PERFORM  20011-HKN-SYUKEI-SEC
           END-IF
      *
      *    総合計
           ADD   1                  TO  SUM-GKENSU(1 IDX3)
           ADD  RECE10-JNISSU (1)   TO  SUM-GNISSU(1 IDX3)
           ADD  RECE10-TOTALTEN(1)  TO  SUM-GTENSU(1 IDX3)
      *
      *
      *    請求先処理（カウントしてない保険者のみ）
           IF  FLG-HKNJACNT        =  ZERO
             IF  RECE10-PREFKBN    =  1
      *        県内処理
               PERFORM  VARYING  IDX4  FROM  1  BY  1
                        UNTIL    HKNJA03-NUM(IDX4) = ZERO
                 IF  WRK-HKNJANUM  =  HKNJA03-HKNJANUM(IDX4)
                   MOVE  1            TO  SUM-KENNAI-FLG(IDX4)
                   ADD   1            TO  SUM-GOKEIHKNJA(1)
                   MOVE  36           TO  IDX4
                 END-IF
               END-PERFORM
             ELSE
      *        県外処理
               EVALUATE  WRK-HKNJANUM (1:2)
                 WHEN  "02"   ADD  1  TO  SUM-KENGAI-KENSU (1)
                 WHEN  "04"   ADD  1  TO  SUM-KENGAI-KENSU (2)
                 WHEN  "05"   ADD  1  TO  SUM-KENGAI-KENSU (3)
                 WHEN  "06"   ADD  1  TO  SUM-KENGAI-KENSU (4)
                 WHEN  "07"   ADD  1  TO  SUM-KENGAI-KENSU (5)
                 WHEN  "12"   ADD  1  TO  SUM-KENGAI-KENSU (6)
                 WHEN  "13"   ADD  1  TO  SUM-KENGAI-KENSU (7)
                 WHEN  "23"   ADD  1  TO  SUM-KENGAI-KENSU (8)
                 WHEN  "09"   ADD  1  TO  SUM-KENGAI-KENSU (9)
                 WHEN  OTHER
                   PERFORM  VARYING  IDX4  FROM  1  BY  1
                            UNTIL    IDX4  >     50
                            OR       SUM-KENMEI(IDX4) = ZERO
                     IF  WRK-HKNJANUM(1:2) = SUM-KENMEI(IDX4)
                       MOVE  IDX4     TO  CNT-KEN
                       MOVE  50       TO  IDX4
                     END-IF
                   END-PERFORM
      *
      *    該当県名がなかった場合
                   IF  IDX4  <  50
                     MOVE  IDX4       TO  CNT-KEN
                     MOVE  WRK-HKNJANUM(1:2)
                                      TO  SUM-KENMEI(CNT-KEN)
                   END-IF
      *
                   ADD  1             TO  SUM-KENGAI-KENSU2(CNT-KEN)
               END-EVALUATE
      *    合計
               ADD      1             TO  SUM-GOKEIHKNJA(2)
             END-IF
           END-IF
      *
      *
      *    集計の対象にならなかったとき
           IF      FLG-UPD             =   1
               PERFORM 20014-UPD-FILE-HENSYU-SEC
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
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
               PERFORM 500-COBABORT-SEC
           END-IF 
           .
       2001-KOUKI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       20011-HKN-SYUKEI-SEC          SECTION.
      *
           IF      IDX1             =   ZERO
           OR      IDX2             =   ZERO
           OR      IDX3             =   ZERO
               PERFORM  20011-HKN-SYUKEI-EXT
           END-IF
      *
      *    療養の給付：件数
           ADD  1                      TO  SUM-KENSU(IDX1 IDX2 IDX3)
      *    療養の給付：診療実日数
           ADD  RECE10-JNISSU(1)       TO  SUM-NISSU(IDX1 IDX2 IDX3)
      *    療養の給付：点数
           ADD  RECE10-TOTALTEN(1)     TO  SUM-TENSU(IDX1 IDX2 IDX3)
      *
           IF  RECE10-SHOKUJINISSU(1)  NOT =  ZERO
      *    食事療養：件数
             ADD  1                    TO  SUM-SKENSU(IDX3)
                                           SUM-GKENSU(2 IDX3)
           END-IF
      *    食事療養：日数
           ADD  RECE10-SHOKUJINISSU(1) TO  SUM-SNISSU(IDX3)
                                           SUM-GNISSU(2 IDX3)
      *    食事療養：食事療養費
           ADD  RECE10-SHOKUJIRYOYOHI(1) TO  SUM-STENSU(IDX3)
                                             SUM-GTENSU(2 IDX3)
      *
           MOVE    1                   TO  FLG-UPD 
           .
       20011-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・集計処理（在総診・在医総）
      *****************************************************************
       20011-ZAI-SYUKEI-SEC       SECTION.
      *
      *    療養の給付：件数
           ADD   1                     TO  SUM-ZKENSU(IDX1 IDX3)
      *
      *    療養の給付：診療実日数
           ADD   RECE10-JNISSU (1)     TO  SUM-ZNISSU(IDX1 IDX3)
      *
      *    療養の給付：点数
           ADD   RECE10-TOTALTEN(1)    TO  SUM-ZTENSU(IDX1 IDX3)
      *
           MOVE    1                   TO  FLG-UPD
           .
       20011-ZAI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求ＤＢ更新用ファイル出力処理
      *****************************************************************
       20014-UPD-FILE-HENSYU-SEC         SECTION.
      *
           MOVE    RECE10-REC          TO  RECE10X-REC
           INITIALIZE                      RECE10X-UPHKNJANUM
                                           RECE10X-XXPREFNUM
                                           RECE10X-XXSRYYM
                                           RECE10X-XXSTHKNJANUM
                                           RECE10X-XXEDHKNJANUM
                                           RECE10X-XXTEISYUTUSAKI
           WRITE   RECE10X-REC
      *
           IF      STS-RECEUPD         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "更新用ファイル 書き込みエラー STS="
                                                   DELIMITED  BY  SIZE
                       STS-RECEUPD                 DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING                                
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF 
           ADD     1                   TO  CNT-UPDFILE
           .
       20014-UPD-FILE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                SECTION.
      *
           MOVE     1           TO       WRK-CNT
           MOVE     ZERO        TO       FLG-PASS
      *
           PERFORM  UNTIL       SUM-KENMEI (WRK-CNT) = ZERO
                    AND         FLG-PASS    =        1
      *      帳票編集<見出し>処理
             PERFORM 20021-HEAD-HEN-SEC
      *      帳票編集<明細>処理
             PERFORM 20023-BODY-HEN-SEC
      *      帳票印刷処理
             PERFORM 20024-PRINT-OUT-SEC
             MOVE    1          TO       FLG-PASS
           END-PERFORM
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し>処理
      *****************************************************************
       20021-HEAD-HEN-SEC          SECTION.
      *
           INITIALIZE                      SKT0305
      *    診療年月
           MOVE    WRK-SRYYMWH         TO  SKT0305-SRYYM
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  SKT0305-ADRS
           INSPECT SKT0305-ADRS        REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  SKT0305-HOSPNAME
           INSPECT SKT0305-HOSPNAME    REPLACING ALL "  " BY "　"
      *    開設者名
           MOVE    WRK-KAISETUNAME     TO  SKT0305-KAISETUNAME
      *    電話番号
           MOVE    WRK-HOSPTEL         TO  SKT0305-TEL
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN    TO  SKT0305-HOSPCD
      *
           .
       20021-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       20023-BODY-HEN-SEC      SECTION.
      *
      *    編集処理
           IF     WRK-CNT          =     1
             PERFORM      VARYING  IDX1  FROM  1  BY  1
                          UNTIL    IDX1  >     2
               PERFORM    VARYING  IDX2  FROM  1  BY  1
                          UNTIL    IDX2  >     2
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                          UNTIL    IDX3  >     2
      *    療養の給付：件数
                   MOVE  SUM-KENSU(IDX1 IDX2 IDX3)  TO  WRK-Z5
                   MOVE  WRK-Z5   TO  SKT0305-KENSU(IDX1 IDX2 IDX3)
      *    療養の給付：日数
                   MOVE  SUM-NISSU(IDX1 IDX2 IDX3)  TO  WRK-Z6
                   MOVE  WRK-Z6   TO  SKT0305-NISSU(IDX1 IDX2 IDX3)
      *    療養の給付：点数
                   MOVE  SUM-TENSU(IDX1 IDX2 IDX3)  TO  WRK-Z9
                   MOVE  WRK-Z9   TO  SKT0305-TENSU(IDX1 IDX2 IDX3)
                 END-PERFORM
               END-PERFORM
             END-PERFORM
      *
      *    在宅処理
             PERFORM      VARYING   IDX1  FROM  1  BY  1
                          UNTIL     IDX1  >     2
               PERFORM    VARYING   IDX2  FROM  1  BY  1
                          UNTIL     IDX2  >     2
      *    療養の給付：件数
                   MOVE  SUM-ZKENSU(IDX1 IDX2)      TO  WRK-Z5
                   MOVE  WRK-Z5   TO  SKT0305-ZKENSU(IDX1 IDX2)
      *    療養の給付：日数
                   MOVE  SUM-ZNISSU(IDX1 IDX2)      TO  WRK-Z6
                   MOVE  WRK-Z6   TO  SKT0305-ZNISSU(IDX1 IDX2)
      *    療養の給付：点数
                   MOVE  SUM-ZTENSU(IDX1 IDX2)      TO  WRK-Z9
                   MOVE  WRK-Z9   TO  SKT0305-ZTENSU(IDX1 IDX2)
               END-PERFORM
             END-PERFORM
      *
      *    合計処理
             PERFORM      VARYING  IDX1  FROM  1  BY  1
                          UNTIL    IDX1  >     2
               PERFORM    VARYING  IDX2  FROM  1  BY  1
                          UNTIL    IDX2  >     2
      *    療養の給付：件数
                 MOVE  SUM-GKENSU(IDX1 IDX2)        TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT0305-GKENSU(IDX1 IDX2)
      *    療養の給付：日数
                 MOVE  SUM-GNISSU(IDX1 IDX2)        TO  WRK-Z7
                 MOVE  WRK-Z7   TO  SKT0305-GNISSU(IDX1 IDX2)
      *    療養の給付：点数
                 MOVE  SUM-GTENSU(IDX1 IDX2)        TO  WRK-Z9
                 MOVE  WRK-Z9   TO  SKT0305-GTENSU(IDX1 IDX2)
               END-PERFORM
             END-PERFORM
      *
      *    請求先名（県内）
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     36
               IF  SUM-KENNAI-FLG (IDX1)  =  1
                 MOVE  "○"       TO  SKT0305-NHKNJA(IDX1)
               END-IF
             END-PERFORM
      *
      *    請求冊数
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     2
               MOVE  SUM-GOKEIHKNJA (IDX1)  TO  WRK-Z3
               MOVE  WRK-Z3     TO       SKT0305-GOKEIHKNJA(IDX1)
             END-PERFORM
      *    請求先名（県外）
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     9
               MOVE  SUM-KENGAI-KENSU (IDX1)  TO  WRK-Z2
               MOVE  WRK-Z2               TO  SKT0305-GHKNJA (IDX1)
             END-PERFORM
      *
           END-IF
      *
      *    県名（その他）
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     20
                    OR       SUM-KENMEI (WRK-CNT) = ZERO
             EVALUATE  SUM-KENMEI (WRK-CNT)
               WHEN  "01"  MOVE  "北海道"  TO  SKT0305-KENMEI (IDX1)
               WHEN  "08"  MOVE  "茨城"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "10"  MOVE  "群馬"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "11"  MOVE  "埼玉"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "14"  MOVE  "神奈川"  TO  SKT0305-KENMEI (IDX1)
               WHEN  "15"  MOVE  "新潟"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "16"  MOVE  "富山"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "17"  MOVE  "石川"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "18"  MOVE  "福井"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "19"  MOVE  "山梨"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "20"  MOVE  "長野"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "21"  MOVE  "岐阜"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "22"  MOVE  "静岡"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "24"  MOVE  "三重"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "25"  MOVE  "滋賀"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "26"  MOVE  "京都"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "27"  MOVE  "大阪"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "28"  MOVE  "兵庫"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "29"  MOVE  "奈良"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "30"  MOVE  "和歌山"  TO  SKT0305-KENMEI (IDX1)
               WHEN  "31"  MOVE  "鳥取"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "32"  MOVE  "島根"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "33"  MOVE  "岡山"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "34"  MOVE  "広島"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "35"  MOVE  "山口"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "36"  MOVE  "徳島"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "37"  MOVE  "香川"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "38"  MOVE  "愛媛"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "39"  MOVE  "高知"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "40"  MOVE  "福岡"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "41"  MOVE  "佐賀"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "42"  MOVE  "長崎"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "43"  MOVE  "熊本"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "44"  MOVE  "大分"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "45"  MOVE  "宮崎"    TO  SKT0305-KENMEI (IDX1)
               WHEN  "46"  MOVE  "鹿児島"  TO  SKT0305-KENMEI (IDX1)
               WHEN  "47"  MOVE  "沖縄"    TO  SKT0305-KENMEI (IDX1)
             END-EVALUATE
             MOVE  SUM-KENGAI-KENSU2(WRK-CNT)  TO  WRK-Z2
             MOVE  WRK-Z2                  TO  SKT0305-GHKNJA2 (IDX1)
             ADD   1                       TO  WRK-CNT
           END-PERFORM
      *
           .
       20023-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       20024-PRINT-OUT-SEC                SECTION.
      *
           DISPLAY  "PRINT"
           ADD     1                   TO  CNT-PAGE
           MOVE    CNT-PAGE            TO  WRK-PAGE
      *
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
           MOVE    WRK-PAGE            TO  SKT0305-NO
           MOVE    "SKT0305.red"         TO  SPRT-PRTID
           MOVE    SKT0305               TO  SPRT-PRTDATA
           DISPLAY "WRK-SYOKBN=" WRK-SYOKBN
           DISPLAY "WRK-PARA-SYOKBN=" WRK-PARA-SYOKBN
           EVALUATE    WRK-SYOKBN      ALSO    WRK-PARA-SYOKBN1
               WHEN    "1"             ALSO    "1"
                   MOVE    "広域連合総括表"        TO  SPRT-TITLE
               WHEN    "2"             ALSO    "1"
                   MOVE    "広域連合総括表［返戻分］" 
                                                   TO  SPRT-TITLE
               WHEN    "1"             ALSO    "2"
                   MOVE    "広域連合総括表［特別療養費］"
                                                   TO  SPRT-TITLE
               WHEN    "2"             ALSO    "2"
                   MOVE    "広域連合総括表［返戻分］［特別療養費］" 
                                               TO  SPRT-TITLE
           END-EVALUATE
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "2"                 TO  SPRT-SITEKBN
      *     
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       20024-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
      *
           INITIALIZE                  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC          =   ZERO
               MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-OUT-INPUT
           ELSE
               MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
           END-IF
      *
           .
       2009-HENKAN-EXT.
           EXIT.
      * 
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
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END     
                                           FLG-ERR
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "SOKATU0305 "         DELIMITED  BY  SIZE
                   WRK-RECEERR         DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING   
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           DISPLAY "*** SOKATU0305 IN   "  CNT-RECE10 
           DISPLAY "***          UPD  "  CNT-UPDFILE 
           DISPLAY "***          PAGE "  CNT-PAGE 
           DISPLAY "*** SOKATU0305 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *                              
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
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
                   MOVE    LNK-PRTKANRI-SRYYM  TO  RECE10-SKYYM
                   MOVE    LNK-PRTKANRI-SKYYMD TO  RECE10-UPDYMD  
                   MOVE    RECE10-REC      TO  MCPDATA-REC
                   MOVE    "DBUPDATE"      TO  MCP-FUNC
                   MOVE    "tbl_seikyu"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC          =   ZERO
                       PERFORM 900-UPD-FILE-READ-SEC
                   ELSE
                       STRING "請求管理ＤＢ　更新エラー　"
                                               DELIMITED  BY  SIZE
                               RECE10-KEY      DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                       END-STRING                                
                       PERFORM 500-ERR-HENSYU-SEC
                       PERFORM 500-COBABORT-SEC
                   END-IF
               ELSE
                   STRING "請求管理ＤＢ　読込み更新エラー　"
                                               DELIMITED  BY  SIZE
                           RECE10X-KEY         DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                   END-STRING                                
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
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
             INITIALIZE                      SRYACT-REC
             INITIALIZE                      FLG-ZAIISO
             INITIALIZE                      FLG-ZAISOSIN
      *
             MOVE  SPA-HOSPNUM           TO  SRY-HOSPNUM
             MOVE  RECE10-NYUGAIKBN      TO  SRY-NYUGAIKBN
             MOVE  RECE10-PTID           TO  SRY-PTID
             MOVE  RECE10-SRYYM          TO  SRY-SRYYM
             MOVE  SRYACT-REC            TO  MCPDATA-REC
             MOVE  "tbl_sryact"          TO  MCP-TABLE
             MOVE  "key4"                TO  MCP-PATHNAME
             PERFORM 900-DBSELECT-SEC
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_sryact"        TO  MCP-TABLE
               MOVE  "key4"              TO  MCP-PATHNAME
               PERFORM  965-SRYACT-READ-SEC
      *    在宅フラグ所持者から在総診の患者取得
               PERFORM  UNTIL  FLG-SRYACT  =  1
                 PERFORM  965-ZAISOU-CHK-SEC
      *
                 MOVE  "tbl_sryact"      TO  MCP-TABLE
                 MOVE  "key4"            TO  MCP-PATHNAME
                 PERFORM  965-SRYACT-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SRYACT SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "tbl_sryact"          TO  MCP-TABLE
             MOVE  "key4"                TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
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
           CALL  "ORCDBMAIN"        USING
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
      *           現状、在総診以外（特医総など）は全部在医総とする
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
           MOVE    ZERO                TO  FLG-RECE10
      *     
      *    請求管理ＤＢ検索
           MOVE    SPACE               TO  RECE10-REC
           INITIALIZE                      RECE10-REC
           MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE10-SKYYM
           MOVE    WRK-CONS-SRYYM-200804
                                       TO  RECE10-SRYYM
           MOVE    999999              TO  RECE10-XXSRYYM
           IF      WRK-PARA-HKNJANUM   NOT =   ZERO
               MOVE    WRK-PARA-HKNJANUM   TO  RECE10-XXSTHKNJANUM
                                               RECE10-XXEDHKNJANUM
           ELSE
               MOVE    SPACE               TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"          TO  RECE10-XXEDHKNJANUM
           END-IF
           IF      WRK-PARA-SYOKBN1    =   "1"
               MOVE    "039"               TO  RECE10-HKNNUM-S
                                               RECE10-HKNNUM
           ELSE 
               MOVE    "040"               TO  RECE10-HKNNUM-S
                                               RECE10-HKNNUM
           END-IF
      *
      *###
           DISPLAY "SYOKBN=" WRK-SYOKBN " SYOKBN1=" WRK-PARA-SYOKBN1
                   " SRYYM=" RECE10-SRYYM "-" RECE10-XXSRYYM 
                   " HKNNUM= " RECE10-HKNNUM
                   " PATHNAME=" WRK-MCP-PATHNAME
                   " HKNJANUM=" RECE10-XXSTHKNJANUM "-" 
                                RECE10-XXEDHKNJANUM
      *###
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-RECE10
               MOVE    HIGH-VALUE      TO  KEY-NEW
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
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-RECE10
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
      *        返戻分でオンライン請求は読み飛ばす
               IF      (WRK-PARA-SYOKBN         =  "2")
               AND     (RECE10-HENREI-SIJI-FLG  =  1)
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *        テスト患者のときは読み飛ばす
               MOVE    SPACE               TO  PTINF-REC
               INITIALIZE                      PTINF-REC
               MOVE    RECE10-HOSPNUM      TO  PTINF-HOSPNUM
               MOVE    RECE10-PTID         TO  PTINF-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 800-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
               AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF   
               ADD     1                     TO  CNT-RECE10   
      *         
               DISPLAY "PTNUM=" RECE10-PTNUM 
      *         
           ELSE
               MOVE    HIGH-VALUE      TO  KEY-NEW
               MOVE    1               TO  FLG-RECE10
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
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
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
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       800-PTINF-READ-EXT.
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
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE           TO  SYS-1900-REC
           INITIALIZE                  SYS-1900-REC
           MOVE    "1900"          TO  SYS-1900-KANRICD
           MOVE    "*"             TO  SYS-1900-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1900-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1900-STYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1900-STYUKYMD(7:2)
           MOVE    SYS-1900-STYUKYMD   TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(17)  NOT =   SPACE
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
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(17)
                                   TO  SYS-1901-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1901-STYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1901-STYUKYMD(7:2)
           MOVE    SYS-1901-STYUKYMD   TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1901-REC
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
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(17)
                                   TO  SYS-1902-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1902-STYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1902-STYUKYMD(7:2)
           MOVE    SYS-1902-STYUKYMD   TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
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
           MOVE    SYS-1900-PRTKBN(17) TO  SYS-1903-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1903-STYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1903-STYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1903-STYUKYMD(7:2)
           MOVE    SYS-1903-STYUKYMD   TO  SYS-1903-EDYUKYMD
           MOVE    SYS-1903-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-KEY10-SEC
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
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-KEY10-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF          MCP-RC      =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SYSKANRI-KEY10-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込（ＮＥＸＴ）
      *****************************************************************
       900-SYSKANRI-READ-N-SEC         SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタ読込
      *****************************************************************
       910-SEIKYU-INV-SEC         SECTION.
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
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
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SEIKYU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者マスター読込（キー）
      *****************************************************************
       900-HKNJAINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNJAINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
