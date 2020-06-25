      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     SEIKYU2407.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 三重・領収証明一覧表
      *                      (161,162,163,164,165)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  10/02/25    吉川          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    10/07/23    他法公費の値は最終診療時の値が
      *                                  記載されるのを修正
      *                                  助成種別毎に印字する
      *  01.00.02    吉川    10/08/05    処方せん発行区分の判定方法変更
      *  01.00.03    吉川    10/11/24    open-cobol1.0対応
      *  01.00.04    吉川    12/07/06    記載順をINIファイルで設定するように修正
      *  01.00.05    萩野    12/09/26    月途中の保険変更でレセプトが分かれても
      *                                  変更前と後の合算した金額を１行で記載する
      *                                  患者番号順で記載されるように修正
      *  01.00.06    吉川    13/01/10    プラグイン対応
      *                                  医療機関情報編集対応
      *  01.00.07    吉川    13/11/07    食事療養費の印字不具合修正
      *                                  プログラムオプション対応
      *  01.00.08    吉川    13/12/05    プログラムオプションの記載順が
      *                                  反映されないのを修正
      *  01.00.09    吉川    14/03/05    帳票様式の変更に伴う修正
      *  01.00.10    吉川    14/04/11    診療年月に年号を追加
      *  01.00.11    萩野    14/05/15    処方せん発行区分対応（診療行為コード追加）
      *  01.00.12    吉川    14/10/23    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.13    吉川    15/07/08    対象公費はプログラムオプション
      *                                  から取得するようにする
      *  01.00.14    吉川    16/01/08    伊勢寡婦（267）は4000点未満、
      *                                  紀宝寡婦（167）と紀宝老人（166）は
      *                                  2667点未満で集計対象外とする
      *                                  医療機関編集情報への開設者名追加対応
      *  01.00.15    萩野    16/05/17    処方せん発行区分対応（診療行為コード追加）
      *  01.00.16    萩野    17/03/08    医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *  01.00.17    吉川    17/06/14    平成２７年１月改正対応
      *  01.00.18    吉川    17/08/14    「公費・長一部負担額」欄は
      *                                  患者負担がない場合はゼロと記載
      *  01.00.19    吉川    18/04/12    70歳以上の高額限度額を考慮する
      *                                  SITEKBN変更
      *  01.00.20    萩野    19/05/23    修正履歴コメント削除
      *                                  改元対応
      *  01.00.21    萩野    19/07/10    松阪市の場合は、高額の有無関係なく
      *                                  一部負担額欄に印字する
      *  01.00.22    井上    19/09/10    松阪市で一部負担金がない場合は一部負担額欄を
      *                                  ０と記載するように修正。
      *  01.00.23    井上    19/09/30    保険番号(261,263)の場合は、負担者番号1〜3桁目が004(松阪市)
      *                                  であるかを判定条件に追加。
      *****************************************************************
      *
       ENVIRONMENT   DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
      *
      *    請求書用ファイル
           SELECT  MF100-FILE   ASSIGN    MF100PARA
                               ORGANIZATION    IS  INDEXED
                               ACCESS  MODE    IS  DYNAMIC
                               RECORD  KEY     IS  MF100-KEY
                               FILE    STATUS  IS  STS-MF100.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE ASSIGN  RECEERR
                                FILE    STATUS  IS  STS-RECEERR.
      *
       DATA  DIVISION.
       FILE  SECTION.
      *
      *    集計ファイル
       FD  MF100-FILE.
       01  MF100-REC.
           COPY  "SEI2407.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
      *
       WORKING-STORAGE  SECTION.
      *
      *    印刷用データ 名称領域
           COPY  "CPCOMMONPRT.INC".
      *
      *    シェル用領域
           COPY  "CPCOMMONSHELL.INC".
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    エラーファイル 名称領域
           COPY  "CPRECEERR.INC".
      *    印刷用データ
           COPY  "SC2407.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-PARA              PIC X(02).
           03  STS-MF100             PIC X(02).
           03  STS-RECE02            PIC X(02).
           03  STS-RECEERR           PIC X(02).
           03  STS-SRT               PIC X(02).
      *
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY            PIC 9(02).
               05  SYS-MM            PIC 9(02).
               05  SYS-DD            PIC 9(02).
           03  SYS-TIME              PIC 9(08).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF             PIC 9(01).
           03  FLG-END               PIC 9(01).
           03  FLG-END1              PIC 9(01).
           03  FLG-SYUNOU            PIC 9(01).
           03  FLG-HKNCOMBI          PIC 9(01).
           03  FLG-HKNNUM            PIC 9(01).
           03  FLG-HKNJAINF          PIC 9(01).
           03  FLG-PTKOHINF          PIC 9(01).
           03  FLG-SYSKANRI          PIC 9(01).
           03  FLG-ERR               PIC 9(01).
           03  FLG-SYORI             PIC 9(01).
           03  FLG-BTPARA            PIC 9(01).
           03  FLG-PRINT             PIC 9(01).
           03  FLG-TOWN              PIC 9(01).
           03  FLG-PASS              PIC 9(01).
           03  FLG-TYOKI             PIC 9(01).
           03  FLG-SRYACT            PIC 9(01).
           03  FLG-SYOHOKOFU         PIC 9(01).
           03  FLG-MF100             PIC 9(01).
           03  FLG-PRGOPTION         PIC 9(01).
           03  FLG-OK                PIC 9(01).
           03  FLG-TAISYOKOHI        PIC 9(01).
           03  FLG-TAISYO            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                   PIC 9(05).
           03  IDY                   PIC 9(04).
           03  IDZ                   PIC 9(04).
           03  IDXX                  PIC 9(04).
           03  IDX1                  PIC 9(04).
           03  IDX3                  PIC 9(04).
           03  IDX4           PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID        PIC 9(07).
           03  WRK-PARA-SHELLID      PIC X(08).
           03  WRK-PARA-HOSPNUM      PIC X(02).
           03  WRK-PARA-PAGE         PIC 9(10).
           03  WRK-PARA-SYORIFLG     PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR           PIC X(200).
           03  WRK-DENPPRTYMWH       PIC X(16).
           03  WRK-SYSYMDWH          PIC X(22).
           03  WRK-SRYYM             PIC X(09).
           03  WRK-SRYKA             PIC 9(02).
      *
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-LASTYMD           PIC 9(08).
           03  WRK-SRYYMWH           PIC X(16).
           03  WRK-SEIYMDWH          PIC X(22).
           03  WRK-HENYMDG           PIC X(22).
           03  WRK-HENYMDG1          PIC X(09).
           03  WRK-HENYMDG2          PIC X(09).
           03  WRK-HKNJANUM          PIC X(05).
           03  WRK-KOHNUM            PIC X(03).
           03  WRK-SRYCD             PIC X(09).
           03  WRK-KEISAN            PIC 9(08).
           03  WRK-AMARI             PIC 9(05).
           03  WRK-KOHID             PIC 9(10).
           03  WRK-KOHTENSU          PIC 9(09).
           03  WRK-TAHOFTN           PIC 9(09).
           03  WRK-ETCSHOKUJINISSU   PIC 9(07).
           03  WRK-ETCSHOKUJIRYOYOHI PIC 9(07).
           03  WRK-ETCSHOKUJIFTN     PIC 9(07).
           03  WRK-SYOHODATA         PIC X(40)  OCCURS 2.
           03  WRK-SRTKBN            PIC X(01).
           03  WRK-HOSPCD            PIC X(10).
      *
           03  WRK-COMP.
               05  WRK-COMP-SRYY        PIC  9(4).
               05  WRK-COMP-BIRY        PIC  9(4).
               05  WRK-COMP-SRYM        PIC  9(2).
               05  WRK-COMP-BIR         PIC  9(2).
      *
      *    編集用エリア
           03  WRK-PAGE         PIC ZZ9.
           03  WRK-Z1           PIC Z.
           03  WRK-Z2           PIC ZZ.
           03  WRK-Z4           PIC ZZZZ.
           03  WRK-Z5           PIC Z,ZZZ.
           03  WRK-Z6           PIC ZZ,ZZZ.
           03  WRK-Z6-9         PIC ZZ,ZZ9.
           03  WRK-Z7           PIC ZZZ,ZZZ.
           03  WRK-Z9           PIC Z,ZZZ,ZZZ.
           03  WRK-Z9-9         PIC Z,ZZZ,ZZ9.
           03  WRK-Z12          PIC ZZZ,ZZZ,ZZZ.
      *
      *    文字変換
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      *
           03  WRK-PATH              PIC X(50).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION        PIC X(100)  OCCURS 100.
           03  WRK-PRGID             PIC X(25).
           03  WRK-KBNCD             PIC X(16).
      *
      *    医療機関情報
           03  WRK-HOSPNAME          PIC X(100).
           03  WRK-HOSPNAME-R        REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1     PIC X(50).
               05  WRK-HOSPNAME2     PIC X(50).
           03  WRK-HOSPADRS          PIC X(100).
           03  WRK-HOSPADRS-R        REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1     PIC X(50).
               05  WRK-HOSPADRS2     PIC X(50).
           03  WRK-HOSPTEL           PIC X(15).
           03  WRK-KAISETUNAME       PIC X(80).
      *
      *    対象公費
           03  WRK-KOHKBN            PIC X(01).
           03  WRK-TAISYO-KOHI  OCCURS 100.
               05  WRK-TAISYO-KBN    PIC X(01).
               05  WRK-TAISYO-KOHNUM PIC X(03).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE    PIC 9(02).
           03  CNT-PAGE    PIC 9(03).
           03  CNT-PRINT   PIC 9(06).
           03  CNT-RECE02  PIC 9(06).
           03  CNT-MF100   PIC 9(06).
      *
      *    キー領域
       01  KEY-AREA  VALUE  ZERO.
           03  KEY-NEW.
               05  KEY-N-CITYNUM     PIC X(03).
               05  KEY-N-KBN         PIC X(01).
           03  KEY-OLD.
               05  KEY-O-CITYNUM     PIC X(03).
               05  KEY-O-KBN         PIC X(01).
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *
      *    比較用公費番号テーブル領域
       01  TBL-HIKAKU-KOHI.
           03  TBL-HKOHNUM              PIC X(03)  OCCURS  4.
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
      *
      *    固定値
       01  CONST-AREA.
      *    処方せんを交付する診療行為
           03  CONS-SYOHOKOFU1       PIC X(09) VALUE "113003510".
           03  CONS-SYOHOKOFU2       PIC X(09) VALUE "113003610".
           03  CONS-SYOHOKOFU3       PIC X(09) VALUE "113003910".
           03  CONS-SYOHOKOFU4       PIC X(09) VALUE "113005810".
           03  CONS-SYOHOKOFU5       PIC X(09) VALUE "113005910".
           03  CONS-SYOHOKOFU6       PIC X(09) VALUE "114007510".
           03  CONS-SYOHOKOFU7       PIC X(09) VALUE "114007610".
           03  CONS-SYOHOKOFU8       PIC X(09) VALUE "114012210".
           03  CONS-SYOHOKOFU9       PIC X(09) VALUE "114013010".
           03  CONS-SYOHOKOFU10      PIC X(09) VALUE "114013210".
           03  CONS-SYOHOKOFU11      PIC X(09) VALUE "160082810".
           03  CONS-SYOHOKOFU12      PIC X(09) VALUE "114019510".
           03  CONS-SYOHOKOFU13      PIC X(09) VALUE "114019710".
           03  CONS-SYOHOKOFU14      PIC X(09) VALUE "114023110".
           03  CONS-SYOHOKOFU15      PIC X(09) VALUE "114023210".
           03  CONS-SYOHOKOFU16      PIC X(09) VALUE "114023510".
           03  CONS-SYOHOKOFU17      PIC X(09) VALUE "114023610".
           03  CONS-SYOHOKOFU18      PIC X(09) VALUE "114023910".
           03  CONS-SYOHOKOFU19      PIC X(09) VALUE "114024010".
           03  CONS-SYOHOKOFU20      PIC X(09) VALUE "114024310".
           03  CONS-SYOHOKOFU21      PIC X(09) VALUE "114024410".
           03  CONS-SYOHOKOFU22      PIC X(09) VALUE "114024910".
           03  CONS-SYOHOKOFU23      PIC X(09) VALUE "114025010".
           03  CONS-SYOHOKOFU24      PIC X(09) VALUE "114025310".
           03  CONS-SYOHOKOFU25      PIC X(09) VALUE "114025410".
           03  CONS-SYOHOKOFU26      PIC X(09) VALUE "114025710".
           03  CONS-SYOHOKOFU27      PIC X(09) VALUE "114025810".
           03  CONS-SYOHOKOFU28      PIC X(09) VALUE "114026110".
           03  CONS-SYOHOKOFU29      PIC X(09) VALUE "114026210".
      *
      *    小児かかりつけ診療料（処方せんを交付）（初診時）
           03  CONS-SYOHOKOFU30      PIC X(09) VALUE "113019710".
      *    小児かかりつけ診療料（処方せんを交付）（再診時） 
           03  CONS-SYOHOKOFU31      PIC X(09) VALUE "113019810".
      *
      *    処方せん無交付加算（在宅時医学総合管理料）
           03  CONS-SYOHOMUKO1       PIC X(09) VALUE "114034370".
      *    処方せん無交付加算（施設入居時等医学総合管理料）
           03  CONS-SYOHOMUKO2       PIC X(09) VALUE "114039170".
      *
      *    在総診テーブル（２０１６年４月診療分〜）
       01  TBL-ZAITAKU6-AREA.
      *    在がん医総（在支診等）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114007610".
      *    在医総・在医総管（ＤＵＭＭＹ）
           03  FILLER          PIC X(09)   VALUE   "099140003".
      *
      *    在がん医総（機能強化した在支診等）（病床あり）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114019510".
      *    在がん医総（機能強化した在支診等）（病床なし）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114019710".
      *
      *    在医総管（機能強化在支診等・病床有・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114030710".
      *    在医総管（機能強化在支診等・病床有・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114030810".
      *    在医総管（機能強化在支診等・病床有・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114030910".
      *    在医総管（機能強化在支診等・病床有・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114031010".
      *    在医総管（機能強化在支診等・病床有・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114031110".
      *    在医総管（機能強化在支診等・病床有・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114031210".
      *    在医総管（機能強化在支診等・病床有・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114031310".
      *    在医総管（機能強化在支診等・病床有・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114031410".
      *    在医総管（機能強化在支診等・病床有・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114031510".
      *    在医総管（機能強化在支診等・病床無・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114031610".
      *    在医総管（機能強化在支診等・病床無・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114031710".
      *    在医総管（機能強化在支診等・病床無・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114031810".
      *    在医総管（機能強化在支診等・病床無・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114031910".
      *    在医総管（機能強化在支診等・病床無・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114032010".
      *    在医総管（機能強化在支診等・病床無・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114032110".
      *    在医総管（機能強化在支診等・病床無・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114032210".
      *    在医総管（機能強化在支診等・病床無・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114032310".
      *    在医総管（機能強化在支診等・病床無・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114032410".
      *    在医総管（在支診等・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114032510".
      *    在医総管（在支診等・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114032610".
      *    在医総管（在支診等・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114032710".
      *    在医総管（在支診等・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114032810".
      *    在医総管（在支診等・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114032910".
      *    在医総管（在支診等・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114033010".
      *    在医総管（在支診等・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114033110".
      *    在医総管（在支診等・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114033210".
      *    在医総管（在支診等・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114033310".
      *    在医総管（在支診等以外・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114033410".
      *    在医総管（在支診等以外・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114033510".
      *    在医総管（在支診等以外・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114033610".
      *    在医総管（在支診等以外・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114033710".
      *    在医総管（在支診等以外・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114033810".
      *    在医総管（在支診等以外・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114033910".
      *    在医総管（在支診等以外・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114034010".
      *    在医総管（在支診等以外・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114034110".
      *    在医総管（在支診等以外・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114034210".
      *
      *    施医総管（機能強化在支診等・病床有・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114035510".
      *    施医総管（機能強化在支診等・病床有・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114035610".
      *    施医総管（機能強化在支診等・病床有・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114035710".
      *    施医総管（機能強化在支診等・病床有・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114035810".
      *    施医総管（機能強化在支診等・病床有・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114035910".
      *    施医総管（機能強化在支診等・病床有・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114036010".
      *    施医総管（機能強化在支診等・病床有・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114036110".
      *    施医総管（機能強化在支診等・病床有・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114036210".
      *    施医総管（機能強化在支診等・病床有・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114036310".
      *    施医総管（機能強化在支診等・病床無・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114036410".
      *    施医総管（機能強化在支診等・病床無・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114036510".
      *    施医総管（機能強化在支診等・病床無・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114036610".
      *    施医総管（機能強化在支診等・病床無・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114036710".
      *    施医総管（機能強化在支診等・病床無・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114036810".
      *    施医総管（機能強化在支診等・病床無・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114036910".
      *    施医総管（機能強化在支診等・病床無・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114037010".
      *    施医総管（機能強化在支診等・病床無・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114037110".
      *    施医総管（機能強化在支診等・病床無・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114037210".
      *    施医総管（在支診等・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114037310".
      *    施医総管（在支診等・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114037410".
      *    施医総管（在支診等・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114037510".
      *    施医総管（在支診等・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114037610".
      *    施医総管（在支診等・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114037710".
      *    施医総管（在支診等・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114037810".
      *    施医総管（在支診等・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114037910".
      *    施医総管（在支診等・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114038010".
      *    施医総管（在支診等・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114038110".
      *    施医総管（在支診等以外・難病等月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114038210".
      *    施医総管（在支診等以外・難病等月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114038310".
      *    施医総管（在支診等以外・難病等月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114038410".
      *    施医総管（在支診等以外・月２回以上・１人）
           03  FILLER          PIC X(09)   VALUE   "114038510".
      *    施医総管（在支診等以外・月２回以上・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114038610".
      *    施医総管（在支診等以外・月２回以上・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114038710".
      *    施医総管（在支診等以外・月１回・１人）
           03  FILLER          PIC X(09)   VALUE   "114038810".
      *    施医総管（在支診等以外・月１回・２〜９人）
           03  FILLER          PIC X(09)   VALUE   "114038910".
      *    施医総管（在支診等以外・月１回・１０人〜）
           03  FILLER          PIC X(09)   VALUE   "114039010".
      *
       01  TBL-ZAITAKU6-R      REDEFINES   TBL-ZAITAKU6-AREA.
            03  TBL-ZAITAKU6   PIC X(09)   OCCURS  79.
      *
       01  TBL-ZAITAKU6-MAX    PIC 9(02)   VALUE   79.
      *
           COPY  "CPSHELLTBL.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト　領域
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY  "CPSK1002.INC".
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
      *    診療科目情報情報
           COPY  "CPSK1005.INC".
      *
      *    状態コメント
           COPY  "CPSK1018.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY  "CPPTINF.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY  "CPPTKOHINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    印刷管理マスタ
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *    印刷マスタ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY    "CPPRGOPTION.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    共通パラメタ
           COPY  "MCPAREA".
      *
           COPY  "MCPDATA.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      ****************************************************************
       LINKAGE  SECTION.
       01  COMMAND-PARAM.
           02  FILLER  PIC X(256).
      *****************************************************************
      *
       PROCEDURE  DIVISION
               USING COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC  SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL  FLG-END = 1
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC  SECTION.
      *
           INITIALIZE  FLG-AREA
           INITIALIZE  STS-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  SPA-AREA
           INITIALIZE  RECEERR
      *
           DISPLAY "START"
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
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-SYORIFLG
                                               WRK-PARA-STSRYYM
                                               WRK-PARA-EDSRYYM
                                               RECEERR
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM  TO  SPA-HOSPNUM
      *
      *    ステップ開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SEIKYU2407"    TO  JOB-PGID
           MOVE    "領収証明一覧表"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    対象年月最終日取得
           INITIALIZE  WRK-SYMD
           MOVE    LNK-PRTKANRI-SRYYM  TO    WRK-SYMD(1:6)
           PERFORM 31012-LASTDAY-GET-SEC
           MOVE    WRK-SYMD         TO    WRK-LASTYMD
      *
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
      *
           INITIALIZE                     SGETTEMP-AREA
      *
           MOVE  WRK-PARA-HOSPNUM     TO  MF100PARA-HOSPNUM
           MOVE  "MF100ZZ"            TO  MF100PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID  TO  MF100PARA-TERMID
      *
           MOVE  MF100PARA-BASENAME   TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEERR              TO  SGETTEMP-BASENAMES (2)
           CALL  "ORCSGETTEMP"        USING
                                      SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  MF100PARA-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEERR
      *
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE  "1001"        TO  SYS-1001-KANRICD
           MOVE  "*"           TO  SYS-1001-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1001-STYUKYMD(1:6)
                                               SYS-1001-EDYUKYMD(1:6)
           END-IF
           MOVE  "01"          TO  SYS-1001-STYUKYMD(7:2)
                                   SYS-1001-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM       TO  SYS-1001-HOSPNUM
           MOVE  SYS-1001-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU2407 SYS 1001 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE  "1002"        TO  SYS-1002-KANRICD
           MOVE  "*"           TO  SYS-1002-KBNCD
           IF  WRK-PARA-STSRYYM  =  "999999"
               MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1002-STYUKYMD(1:6)
                                               SYS-1002-EDYUKYMD(1:6)
           END-IF
           MOVE  "01"          TO  SYS-1002-STYUKYMD(7:2)
                                   SYS-1002-EDYUKYMD(7:2)
           MOVE  SPA-HOSPNUM       TO  SYS-1002-HOSPNUM
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU2403 SYS 1002 ERR ***"
             MOVE  1            TO  FLG-END
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
      *    パラメタ編集処理
           PERFORM 1001-PARA-HENSYU-SEC
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
           PERFORM 900-KOHSKY-START-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       1001-PARA-HENSYU-SEC        SECTION.
      *
           MOVE    ZERO                    TO  IDXX
                                           FLG-SYORI
           INITIALIZE                      TABLE-AREA
      *
           IF      WRK-PARA-SYORIFLG   =   "1" 
               INITIALIZE                      BTPARA-REC
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
               MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
               MOVE    BTPARA-REC          TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM 900-BTPARA-READ-N-SEC
               ELSE
                   INITIALIZE                  BTPARA-REC
                   MOVE    1               TO  FLG-BTPARA
               END-IF
      *
               PERFORM     UNTIL   FLG-BTPARA  =   1
                   ADD     1                   TO  IDXX
                   IF      BTPARA-INFO-KBN     =   "1"
                     MOVE    1                 TO  FLG-SYORI
                     MOVE    BTPARA-INFO-PARA
                                       TO TBL-PARA-HKNJANUM(IDXX)
                   ELSE
                     MOVE    2                 TO  FLG-SYORI
                     MOVE    BTPARA-INFO-PTID
                                       TO TBL-PARA-PTID(IDXX)
                   END-IF
                   DISPLAY "パラメタ区分=" BTPARA-INFO-KBN
                           "パラメタ=" BTPARA-INFO-PARA
                           "保険者番号="   TBL-PARA-HKNJANUM(IDXX)
                           "患者番号="     TBL-PARA-PTID(IDXX)
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
                   PERFORM 900-BTPARA-READ-N-SEC
               END-PERFORM
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
               IF      IDXX            =   ZERO
                   MOVE    ZERO            TO  WRK-PARA-SYORIFLG
               END-IF
           END-IF 
           .
       1001-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SEIKYU2407"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    記載順
           MOVE    "SORT"                    TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           IF  WRK-OPTION(1)(1:1)  NOT =  "2"
               MOVE    "1"                   TO  WRK-SRTKBN
           ELSE
               MOVE    WRK-OPTION(1)(1:1)    TO  WRK-SRTKBN
           END-IF
           DISPLAY "ソート区分="  WRK-SRTKBN
      *
      *    対象公費番号
           MOVE    "TAISYOKOHI"              TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  100
                    OR       WRK-OPTION(IDX)  =  SPACE
             UNSTRING   WRK-OPTION(IDX)   DELIMITED  BY  ":"
                                          INTO   WRK-TAISYO-KBN(IDX)
                                                 WRK-TAISYO-KOHNUM(IDX)
             END-UNSTRING
             DISPLAY "対象公費区分="  WRK-TAISYO-KBN(IDX)
             DISPLAY "対象公費番号="  WRK-TAISYO-KOHNUM(IDX)
           END-PERFORM
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタセット処理
      *****************************************************************
       1001-OPTION-SET-SEC              SECTION.
      *
           INITIALIZE                            WRK-OPTION-TBL
                                                 FLG-OK
      *    プログラムオプション情報読込
           PERFORM  900-PRGOPTION-READ-SEC
      *
      *    オプション数の初期設定
           MOVE    1                         TO  IDX1
           IF  FLG-PRGOPTION  =  ZERO
             PERFORM  VARYING  IDX  FROM  1  BY  1
                      UNTIL   (IDX     >   44000 )
                         OR   (IDX1    >  100)
                         OR   (FLG-OK  NOT =   ZERO)
      *
      *        オプションの1桁目が空白であれば処理終了
               IF   PRGOPTION-OPTION(IDX:1)  =  SPACE
               AND  IDY                      =  ZERO
                   MOVE    1                 TO  FLG-OK
               END-IF
      *
               IF  FLG-OK  =  ZERO
      *          改行地点か
                 IF  PRGOPTION-OPTION(IDX:1)  =   X"0A"
                 OR  IDY  >=  100
                   ADD    1                  TO  IDX1
                   MOVE    ZERO              TO  IDY
                 ELSE
                   ADD    1                  TO  IDY
      *            PGオプション内容を１文字ずつセット
                   MOVE    PRGOPTION-OPTION(IDX:1)
                                         TO  WRK-OPTION(IDX1)(IDY:1)
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           .
       1001-OPTION-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC  SECTION.
      *
           OPEN     OUTPUT  MF100-FILE
           CLOSE            MF100-FILE
           OPEN     I-O     MF100-FILE
      *
           MOVE     ZERO    TO      FLG-END
      *
           PERFORM  2001-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   MF100-FILE
      *
           IF       CNT-RECE02   >       0
                    PERFORM   2002-PRINT-SEC
           END-IF
            MOVE   1   TO   FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理１
      *****************************************************************
       2001-SYUKEI-SEC  SECTION.
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       4
             INITIALIZE                            TBL-HIKAKU-KOHI
             MOVE    RECE02-KOHNUM(IDZ)        TO  TBL-HKOHNUM(1)
      *      対象公費番号チェック処理
             PERFORM  800-TAISYOKOHI-CHK-SEC
             IF  FLG-TAISYO  =  1
                 PERFORM  20011-SYUKEI-SEC
             END-IF
           END-PERFORM
      *
           PERFORM 900-KOHSKY-READ-SEC
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      **
      *****************************************************************
      *    集計処理
      *****************************************************************
       20011-SYUKEI-SEC  SECTION.
      *
           INITIALIZE                    MF100-REC
      *
           MOVE  SPA-HOSPNUM                 TO  MF100-HOSPNUM
           MOVE  RECE02-PTNUM                TO  MF100-PTNUM
           MOVE  RECE02-RECEKA               TO  MF100-SRYKA
           MOVE  RECE02-HKNJANUM-KEY         TO  MF100-HKNJANUM-KEY
           MOVE  RECE02-TEKSTYMD             TO  MF100-TEKSTYMD
           MOVE  RECE02-NYUGAIKBN            TO  MF100-NYUGAIKBN
      *
           MOVE  RECE02-NAME                 TO  MF100-NAME
           MOVE  RECE02-SEX                  TO  MF100-SEX
           MOVE  RECE02-BIRTHDAY             TO  MF100-BIRTHDAY
           MOVE  RECE02-HKN                  TO  MF100-HKN
           MOVE  RECE02-SKYYM                TO  MF100-SKYYM
           MOVE  RECE02-SRYYM                TO  MF100-SRYYM
      *
      *
      *    TMP-FILE キーセット
      *    市町村番号
           MOVE  RECE02-KOHFTNJANUM(IDZ)     TO   MF100-CITYNUM
      *
      *    助成種別
           MOVE     WRK-KOHKBN               TO   MF100-KOHKBN
      *
      *    SORT.INIによって記載順を変える(1=受給者番号順、2=患者番号順)
           EVALUATE  WRK-SRTKBN
             WHEN  "1"
               MOVE    RECE02-KOHJKYSNUM(IDZ)  TO   MF100-SORTNUM
             WHEN  "2"
               MOVE    RECE02-PTNUM            TO   MF100-SORTNUM
           END-EVALUATE
      *
      *    集計ファイル読込
           PERFORM  900-SKTTMP-READ-SEC
      *
           MOVE  RECE02-KOHINF      (IDZ)    TO  MF100-KOHINF
           MOVE  RECE02-KOHFTNJANUM (IDZ)    TO  MF100-FTNJANUM
           MOVE  RECE02-KOHJKYSNUM  (IDZ)    TO  MF100-KOHJKYSNUM
           COMPUTE IDY     =       IDZ     +   1
           ADD  RECE02-JNISSU       (IDY)    TO  MF100-JNISSU
           ADD  RECE02-TOTALTEN     (IDY)    TO  MF100-TOTALTEN
           ADD  RECE02-FTNMONEY     (IDY)    TO  MF100-FTNMONEY
           ADD  RECE02-SHOKUJINISSU (1)      TO  MF100-SHOKUJINISSU
           ADD  RECE02-SHOKUJIRYOYOHI(1)     TO  MF100-SHOKUJIRYOYOHI
           ADD  RECE02-SHOKUJIFTN   (1)      TO  MF100-SHOKUJIFTN
      *
      *    収納情報より他法公費情報を取得
           MOVE     MF100-KOHNUM             TO   WRK-KOHNUM  
           MOVE     MF100-KOHID              TO   WRK-KOHID
           MOVE     ZERO                     TO   MF100-FTNMONEY 
                                                  MF100-TAHOFTN
           IF  RECE02-NYUGAIKBN  =  "1"
             MOVE    "key33"                 TO  WRK-PATH
           ELSE
             MOVE    "key25"                 TO  WRK-PATH
           END-IF
           PERFORM  900-SYUNOU-SET-SEC
      *
      *    長期
           IF   FLG-TYOKI                     =   1
           AND  MF100-TAHOFTN                >=   10000
             MOVE   10000                    TO   MF100-TAHOFTN
             MOVE   1                        TO   MF100-TYOKI
           END-IF
      *
      *    長期上位
           IF   FLG-TYOKI                     =   2
           AND  MF100-TAHOFTN                >=   20000
             MOVE   20000                    TO   MF100-TAHOFTN
             MOVE   2                        TO   MF100-TYOKI
           END-IF
      *
      *    高額療養費
           IF  RECE02-ZAITAKU  =  "2" OR "3" OR "4"
                               OR "5" OR "6"
             IF  RECE02-FTNMONEY(1)  >  ZERO
               MOVE    RECE02-FTNMONEY(1)    TO  MF100-FTNMONEY
             END-IF
           END-IF
      *
           IF  (RECE02-ZAITAKU  =  SPACE)
           AND (RECE02-AGE     >=  70)
               IF  RECE02-FTNMONEY(1)  >  ZERO
                   MOVE    RECE02-FTNMONEY(1)   TO  MF100-FTNMONEY
               END-IF
           END-IF
      *
      *    松阪市の福祉の場合は、高額の有無関係なく福祉の負担金を印字する
           IF  MF100-KOHNUM            = (( "261" OR  "263" )
           AND ( MF100-CITYNUM(1:3)    =    "004" ))
           OR  ( MF100-KOHNUM          =    "262" OR  "361" OR  "363" )
               MOVE  RECE02-FTNMONEY    (IDY)    TO  MF100-FTNMONEY
           END-IF
      *
      *    レセプトの「処方せん」に点数がある場合、発行区分に1をセット
           IF  RECE02-TENSU(33 IDY)  >  ZERO
             MOVE   1                 TO  FLG-SYOHOKOFU
           ELSE
      *      処方せんを交付する診療行為があるかチェック
             PERFORM 930-SRYACT-SET-SEC
           END-IF
           IF  FLG-SYOHOKOFU  =  1
             MOVE   1                        TO   MF100-SYOHO
           END-IF
      *
      *    保険者番号チェック処理
           MOVE    ZERO               TO   FLG-ERR
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
           IF  FLG-ERR    =    ZERO
             IF  FLG-MF100  =  1
                 DISPLAY "新規"
                 WRITE   MF100-REC
                 ADD     1       TO  CNT-MF100
             ELSE
                 DISPLAY "上書き"
                 REWRITE  MF100-REC
             END-IF
           ELSE
             DISPLAY "助成対象外患者番号="  RECE02-PTNUM
                     "公費番号="            MF100-KOHNUM
                     "診療年月="            MF100-SRYYM
           END-IF
           .
       20011-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別発行時の保険者番号(市町村番号)チェック処理
      *****************************************************************
       200111-HKNJANUM-CHK-SEC                SECTION.
      *
           INITIALIZE                           WRK-HKNJANUM
           MOVE    "24"                     TO  WRK-HKNJANUM(1:2)
           MOVE    MF100-CITYNUM(1:3)       TO  WRK-HKNJANUM(3:3)
      *
           IF  WRK-PARA-SYORIFLG       =   "1"
               AND FLG-SYORI           =    1
               AND FLG-ERR             =   ZERO
      *
               DISPLAY "患者番号 = "  MF100-PTNUM
               DISPLAY "チェック用保険者番号="  WRK-HKNJANUM
               MOVE    1       TO      FLG-ERR
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                       OR      TBL-PARA-HKNJANUM(IDXX)  =   SPACE
                   IF      WRK-HKNJANUM(1:5)
                                   =   TBL-PARA-HKNJANUM(IDXX)(1:5)
                       MOVE    ZERO                TO  FLG-ERR
                       MOVE    WRK-CONS-PARA-MAX   TO  IDXX
                   END-IF
               END-PERFORM 
           END-IF
      *
           .
       200111-HKNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC  SECTION.
      *
           INITIALIZE  IDX1
      *
           OPEN     INPUT       MF100-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    レセプト明細読込
           PERFORM  900-MF100-READ-SEC
      *
           PERFORM   UNTIL  FLG-PRINT  =  1
      *
      *       キーセット
              MOVE  KEY-NEW    TO  KEY-OLD
      *
      *       帳票編集<見出し>処理
              PERFORM  310-HEAD-HEN-SEC
      *
      *       帳票編集<明細>処理
              PERFORM  320-BODY-HEN-SEC
      *
      *       帳票印刷処理
              PERFORM  330-PRINT-OUT-SEC
      *
           END-PERFORM
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC  SECTION.
      *
           ADD    1                         TO  CNT-PAGE
      *
           INITIALIZE                           SC07
      *
      *    市町村名
           MOVE    "24"                     TO  WRK-HKNJANUM(1:2)
           MOVE    MF100-CITYNUM            TO  WRK-HKNJANUM(3:3)
           PERFORM  920-HKNJAINF-SET-SEC
           STRING  HKNJA-HKNJANAME              DELIMITED BY SPACE
                   "長"                         DELIMITED BY SIZE
                   INTO                         SC07-CITYNAME
           END-STRING
      *
      *    請求年月日
           MOVE    WRK-SEIYMDWH             TO  SC07-SEIYMD
      *
      *    医療機関コード
           STRING  "24"                     DELIMITED  BY  SIZE
                   SYS-1001-TENHYOKBN       DELIMITED  BY  SIZE
                   SYS-1001-HOSPCD          DELIMITED  BY  SIZE
                   INTO                     WRK-HOSPCD
           END-STRING
           MOVE    WRK-HOSPCD               TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT            TO  SC07-HOSPCD
      *
      *    郵便番号
           MOVE    "〒"                     TO  SC07-POST (1:)
           MOVE    SYS-1002-POST (1:3)      TO  SC07-POST (3:)
           MOVE    "-"                      TO  SC07-POST (6:)
           MOVE    SYS-1002-POST (4:)       TO  SC07-POST (7:)
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS             TO  SC07-ADRS
           INSPECT SC07-ADRS                REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME             TO  SC07-HOSPNAME
           INSPECT SC07-HOSPNAME            REPLACING ALL "  " BY "　"
      *
      *    開設者氏名
           MOVE    WRK-KAISETUNAME          TO  SC07-KAISETU
      *
      *    電話番号
           MOVE    WRK-HOSPTEL              TO  SC07-TEL
      *
      *    ページ番号
           MOVE    CNT-PAGE                 TO  WRK-PAGE
           MOVE    WRK-PAGE                 TO  SC07-PAGE
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC  SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >   15
                   OR  KEY-NEW     NOT =   KEY-OLD
                   OR  FLG-PRINT       =   1
      *
      *    市町村コード
             MOVE  MF100-CITYNUM        TO  SC07-CITYCD (IDX1)
      *
      *    助成種別
             MOVE  MF100-KOHKBN         TO  SC07-KOHKBN (IDX1)
      *
      *    受給資格証記号番号（受給者番号）
             MOVE  MF100-KOHJKYSNUM     TO  SC07-JKYSNUM (IDX1)
      *
      *    氏名
             MOVE  MF100-NAME           TO  SC07-NAME (IDX1)
      *
      *    性別
             MOVE  MF100-SEX            TO  SC07-SEX (IDX1)
      *
      *    生年月日
             MOVE   MF100-BIRTHDAY      TO  WRK-SYMD
             PERFORM 31011-SEIWA-HEN-SEC
             MOVE     LNK-DAY1-GENGO    TO  SC07-BIRTHDAY (IDX1)(1:1)
             PERFORM  31012-SEIWA-HEN-SEC
             MOVE   WRK-HENYMDG1(2:2)   TO  SC07-BIRTHDAY (IDX1)(2:2)
             MOVE   WRK-HENYMDG1(5:2)   TO  SC07-BIRTHDAY (IDX1)(4:2)
             MOVE   WRK-HENYMDG1(8:2)   TO  SC07-BIRTHDAY (IDX1)(6:2)
             IF  WRK-HENYMDG1(2:1)  =  SPACE
               MOVE  ZERO               TO  SC07-BIRTHDAY (IDX1)(2:1)
             END-IF
             IF  WRK-HENYMDG1(5:1)  =  SPACE
               MOVE  ZERO               TO  SC07-BIRTHDAY (IDX1)(4:1)
             END-IF
             IF  WRK-HENYMDG1(8:1)  =  SPACE
               MOVE  ZERO               TO  SC07-BIRTHDAY (IDX1)(6:1)
             END-IF
      *
      *    診療年月
             MOVE  MF100-SRYYM          TO  WRK-SYMD (1:6)
             MOVE  "01"                 TO  WRK-SYMD (7:2)
             PERFORM 31011-SEIWA-HEN-SEC
             MOVE     LNK-DAY1-GENGO    TO  SC07-SRYYM (IDX1)(1:1)
             PERFORM 31012-SEIWA-HEN-SEC
             MOVE  WRK-HENYMDG1         TO  WRK-SRYYM
             MOVE  WRK-SRYYM (2:2)      TO  SC07-SRYYM (IDX1)(2:2)
             MOVE  WRK-SRYYM (5:2)      TO  SC07-SRYYM (IDX1)(4:2)
             IF  WRK-SRYYM(2:1)  =  SPACE
               MOVE  ZERO               TO  SC07-SRYYM (IDX1)(2:1)
             END-IF
             IF  WRK-SRYYM(5:1)  =  SPACE
               MOVE  ZERO               TO  SC07-SRYYM (IDX1)(4:1)
             END-IF
      *
      *    給付割合
             MOVE  MF100-KYURATE(2:1)   TO  SC07-RATE (IDX1)
      *
      *    入外区分
             IF  MF100-NYUGAIKBN  =  "1"
               MOVE  MF100-NYUGAIKBN    TO  SC07-NYUGAIKBN(IDX1)
             END-IF
      *
      *    入院日数
             IF  MF100-NYUGAIKBN  =  "1"
               MOVE  MF100-JNISSU       TO  WRK-Z2
               MOVE  WRK-Z2             TO  SC07-NISSU (IDX1)
             END-IF
      *
      *    保険点数
             MOVE  MF100-TOTALTEN       TO  WRK-Z7
             MOVE  WRK-Z7               TO  SC07-TENSU (IDX1)
      *
      *    一部負担額
             IF  MF100-KOHNUM            = (( "261" OR "263" )
             AND ( MF100-CITYNUM(1:3)    =    "004" ))
             OR  ( MF100-KOHNUM          =    "262" OR "361" OR "363" )
               MOVE  MF100-FTNMONEY       TO  WRK-Z9-9
               MOVE  WRK-Z9-9             TO  SC07-ITBFTN (IDX1)
             ELSE
               MOVE  MF100-FTNMONEY       TO  WRK-Z9
               MOVE  WRK-Z9               TO  SC07-ITBFTN (IDX1)
             END-IF
      *
      *    公費・長期区分
             MOVE  MF100-TAHO           TO  WRK-KOHNUM
             PERFORM  950-HKNNUM-SET-SEC
             MOVE  HKN-HBTNUM           TO  SC07-TYOKI (IDX1)
      *
      *    公費点数
             IF  MF100-TAHOTEN  >  MF100-TOTALTEN
               MOVE  MF100-TOTALTEN     TO  WRK-Z7
             ELSE
               MOVE  MF100-TAHOTEN      TO  WRK-Z7
             END-IF
             MOVE  WRK-Z7               TO  SC07-ETENSU(IDX1)
      *
      *    公費・長期一部負担額
             IF  MF100-TAHO  NOT =  SPACE
             OR  MF100-TYOKI  =  1
                 MOVE  MF100-TAHOFTN    TO  WRK-Z6-9
                 MOVE  WRK-Z6-9         TO  SC07-EFTNMONEY(IDX1)
             ELSE
                 MOVE  MF100-TAHOFTN    TO  WRK-Z6
                 MOVE  WRK-Z6           TO  SC07-EFTNMONEY(IDX1)
             END-IF
      *
      *    食事療養費（保険請求分）
             MOVE  MF100-SHOKUJIRYOYOHI
                                        TO  WRK-Z9
             MOVE  WRK-Z9               TO  SC07-SHOKUJIRYOYOHI (IDX1)
      *
      *    食事標準負担額
             MOVE  MF100-SHOKUJIFTN     TO  WRK-Z7
             MOVE  WRK-Z7               TO  SC07-SHOKUJIFTN (IDX1)
      *
      *    食事療養費（公費請求分）
             MOVE  MF100-ETCSHOKUJIRYOYOHI
                                        TO  WRK-Z9
             MOVE  WRK-Z9               TO  SC07-ESHOKUJIRYOYOHI(IDX1)
      *
      *    食事公費標準負担額
             MOVE  MF100-ETCSHOKUJIFTN  TO  WRK-Z7
             MOVE  WRK-Z7               TO  SC07-ESHOKUJIFTN (IDX1)
      *
      *    処方箋発行区分
             IF  MF100-SYOHO  =  1
                 MOVE     "1"           TO   SC07-SYOHO (IDX1)
             END-IF
      *
      *    レセプト明細読込
             PERFORM  900-MF100-READ-SEC
      *
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       330-PRINT-OUT-SEC  SECTION.
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
           MOVE   "SC2407.red"         TO  SPRT-PRTID
           MOVE    "領収証明一覧表"    TO  SPRT-TITLE
           MOVE    SC07                TO  SPRT-PRTDATA
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
               MOVE    "帳票ＤＢに更新できませんでした"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
                 .
       330-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC  SECTION.
      *
           OPEN  INPUT  RECEERR-FILE
           IF  STS-RECEERR  =  ZERO
              CONTINUE
           ELSE
              OPEN  OUTPUT  RECEERR-FILE
      *
              MOVE  WRK-RECEERR  TO  RECEERR-REC
              WRITE RECEERR-REC
              CLOSE  RECEERR-FILE
      *         
      *        ジョブ終了処理
              MOVE    "JBE"           TO  SJOBKANRI-MODE
              INITIALIZE                  JOBKANRI-REC
              MOVE    SPA-HOSPNUM
                                      TO  JOB-HOSPNUM
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC   SECTION.
      *
           CLOSE  MF100-FILE
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU2407 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU2407 OUT "  CNT-MF100 
           DISPLAY "*** SEIKYU2407 END ***"
      *
           IF  CNT-MF100  >  0
              CONTINUE
           ELSE
              DISPLAY "DATA NOT FOUND. PRINT JOB CANCEL!!"
           END-IF
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
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦・和暦変換処理
      *****************************************************************
       31011-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
      *
           .
       31011-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC  SECTION.
      *
           INITIALIZE  STS-AREA-DAY
           INITIALIZE  LNK-DAY2-AREA
           MOVE  "21"      TO  LNK-DAY2-IRAI
           MOVE  WRK-SYMD  TO  LNK-DAY2-YMD
           CALL  "ORCSDAY" USING  STS-AREA-DAY
                                  LNK-DAY2-AREA
           MOVE  LNK-DAY2-EDTYMD3  TO  WRK-HENYMDG
           MOVE  LNK-DAY2-EDTYMD1  TO  WRK-HENYMDG1
           INSPECT WRK-HENYMDG REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象公費番号チェック処理
      *****************************************************************
       800-TAISYOKOHI-CHK-SEC                SECTION.
      *
           INITIALIZE     FLG-TAISYO
      *
           PERFORM  VARYING  IDXX  FROM  1  BY  1
                    UNTIL    IDXX  >  100
                    OR       WRK-TAISYO-KOHNUM(IDXX)  =  SPACE
             IF  TBL-HKOHNUM(1)  =  WRK-TAISYO-KOHNUM(IDXX)
             OR  TBL-HKOHNUM(2)  =  WRK-TAISYO-KOHNUM(IDXX)
             OR  TBL-HKOHNUM(3)  =  WRK-TAISYO-KOHNUM(IDXX)
             OR  TBL-HKOHNUM(4)  =  WRK-TAISYO-KOHNUM(IDXX)
               MOVE     WRK-TAISYO-KBN(IDXX)
                                            TO  WRK-KOHKBN
               MOVE     1                   TO  FLG-TAISYO
               MOVE     100                 TO  IDXX
             END-IF
           END-PERFORM
      *
           .
       800-TAISYOKOHI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計ファイル読込
      *****************************************************************
       900-SKTTMP-READ-SEC             SECTION.
      *
           READ    MF100-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-MF100
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-MF100
           END-READ
           .
       900-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタ読込
      *****************************************************************
       900-KOHSKY-START-SEC            SECTION.
      *
           INITIALIZE                  RECE02-REC
           MOVE    SPA-HOSPNUM         TO  RECE02-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE02-SKYYM
           MOVE    RECE02-REC          TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
              MOVE    "tbl_kohsky"        TO  MCP-TABLE
              MOVE    "key2"              TO  MCP-PATHNAME
              PERFORM 900-KOHSKY-READ-SEC
           ELSE
              MOVE    1                   TO  FLG-END
              PERFORM 900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-START-EXT.
           EXIT.
      *      
      *****************************************************************
      *    公費請求書マスタＲＥＡＤ
      *****************************************************************
       900-KOHSKY-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"                TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
               INITIALIZE                            TBL-HIKAKU-KOHI
               MOVE    RECE02-KOHNUM(1)          TO  TBL-HKOHNUM(1)
               MOVE    RECE02-KOHNUM(2)          TO  TBL-HKOHNUM(2)
               MOVE    RECE02-KOHNUM(3)          TO  TBL-HKOHNUM(3)
               MOVE    RECE02-KOHNUM(4)          TO  TBL-HKOHNUM(4)
      *        対象公費番号チェック処理
               PERFORM  800-TAISYOKOHI-CHK-SEC
               IF  FLG-TAISYO  =  1
      *
                   IF      WRK-PARA-STSRYYM    =   "999999"
                       CONTINUE
                   ELSE
                       IF  ( RECE02-SRYYM      <   WRK-PARA-STSRYYM )
                       OR  ( RECE02-SRYYM      >   WRK-PARA-EDSRYYM )
                           GO  TO  900-KOHSKY-READ-SEC
                       END-IF
                   END-IF
      *
      *        妊産婦で自己負担額が1500円未満の場合は読み飛ばす
                 IF  RECE02-KOHNUM (1)  =  "165" OR
                     RECE02-KOHNUM (2)  =  "165" OR
                     RECE02-KOHNUM (3)  =  "165" OR
                     RECE02-KOHNUM (4)  =  "165"
                  IF  (RECE02-KYURATE  = 30  AND
                       RECE02-TOTALTEN(1)  <  501)
                  OR  (RECE02-KYURATE  = 20  AND
                       RECE02-TOTALTEN(1)  <  751)
                  OR  (RECE02-KYURATE  = 10  AND
                       RECE02-TOTALTEN(1)  <  1501)
                    GO  TO  900-KOHSKY-READ-SEC
                  END-IF
                END-IF
      *
      *        68・69歳老人で自己負担額が3200円未満の場合は読み飛ばす
                 IF  RECE02-KOHNUM (1)  =  "164" OR
                     RECE02-KOHNUM (2)  =  "164" OR
                     RECE02-KOHNUM (3)  =  "164" OR
                     RECE02-KOHNUM (4)  =  "164"
                  IF  (RECE02-KYURATE  = 30  AND
                       RECE02-TOTALTEN(1)  <  1067)
                  OR  (RECE02-KYURATE  = 20  AND
                       RECE02-TOTALTEN(1)  <  1601)
                  OR  (RECE02-KYURATE  = 10  AND
                       RECE02-TOTALTEN(1)  <  3201)
                    GO  TO  900-KOHSKY-READ-SEC
                  END-IF
                END-IF
      *
      *        紀宝寡婦、紀宝老人で点数が2667点未満の場合は読み飛ばす
                 IF  RECE02-KOHNUM (1)  =  "166" OR "167"
                 OR  RECE02-KOHNUM (2)  =  "166" OR "167"
                 OR  RECE02-KOHNUM (3)  =  "166" OR "167"
                 OR  RECE02-KOHNUM (4)  =  "166" OR "167"
                  IF  RECE02-TOTALTEN(1)  <  2667
                    GO  TO  900-KOHSKY-READ-SEC
                  END-IF
                END-IF
      *
      *        伊勢寡婦で点数が4000点未満の場合は読み飛ばす
                 IF  RECE02-KOHNUM (1)  =  "267" OR
                     RECE02-KOHNUM (2)  =  "267" OR
                     RECE02-KOHNUM (3)  =  "267" OR
                     RECE02-KOHNUM (4)  =  "267"
                  IF  RECE02-TOTALTEN(1)  <  4000
                    GO  TO  900-KOHSKY-READ-SEC
                  END-IF
                END-IF
      *
      *        テスト患者のときは読み飛ばす
                 MOVE    SPACE               TO  PTINF-REC
                 INITIALIZE                      PTINF-REC
                 MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                 MOVE    RECE02-PTID         TO  PTINF-PTID
                 MOVE    PTINF-REC           TO  MCPDATA-REC
                 PERFORM 800-PTINF-READ-SEC
                 IF      FLG-PTINF           =   ZERO
                 AND     PTINF-TSTPTNUMKBN   =   "1"
                     GO  TO  900-KOHSKY-READ-SEC
                 END-IF    
      * 
      *        個別発行で患者番号指定の場合、対象判定を行う
                   MOVE  ZERO                TO  FLG-ERR
                   IF      WRK-PARA-SYORIFLG   =   "1"
                   AND     FLG-SYORI           =    2
                     MOVE  1                   TO  FLG-ERR
                     PERFORM VARYING IDXX    FROM    1   BY  1
                             UNTIL   IDXX    >   WRK-CONS-PARA-MAX
                             OR      TBL-PARA-PTID(IDXX) =   ZERO
                         IF      RECE02-PTID =   TBL-PARA-PTID(IDXX)
                           MOVE    ZERO                TO  FLG-ERR
                           MOVE    WRK-CONS-PARA-MAX   TO  IDXX
                         END-IF
                     END-PERFORM 
                   END-IF
      *
                   IF      FLG-ERR           =   1
                     GO  TO  900-KOHSKY-READ-SEC
                   END-IF 
      *
                 ADD  1  TO  CNT-RECE02
               ELSE
                 GO  TO  900-KOHSKY-READ-SEC
               END-IF
           ELSE
               MOVE    1               TO  FLG-END
               PERFORM 900-KOHSKY-CLOSE-SEC
           END-IF
      *
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタCLOSE
      *****************************************************************
       900-KOHSKY-CLOSE-SEC        SECTION.
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"                  TO  MCP-FUNC
           MOVE    "tbl_ptinf"                 TO  MCP-TABLE
           MOVE    "key"                       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                 USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
               MOVE    "tbl_ptinf"             TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"             USING
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
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
           MOVE    "tbl_ptinf"                 TO  MCP-TABLE
           MOVE    "key"                       TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                 USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報ＲＥＡＤ  
      *****************************************************************
       901-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
              MOVE    ZERO                TO  FLG-END1
              MOVE    MCPDATA-REC         TO  PTINF-REC
           ELSE
              MOVE    1               TO  FLG-END1
           END-IF
           .
       901-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書読込
      *****************************************************************
       900-MF100-READ-SEC  SECTION.
      *
           READ  MF100-FILE  NEXT
              AT  END
                 MOVE  1                   TO  FLG-PRINT
                 MOVE  HIGH-VALUE          TO  KEY-N-CITYNUM
                 MOVE  HIGH-VALUE          TO  KEY-N-KBN
              NOT AT  END
                 MOVE  MF100-CITYNUM       TO  KEY-N-CITYNUM
                 MOVE  MF100-KOHKBN        TO  KEY-N-KBN
           END-READ
           .
       900-MF100-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SYU-PTID            TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"                TO  MCP-FUNC
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                 TO  MCP-FUNC
               MOVE    "tbl_hkncombi"        TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"           USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE                            HKNJAINF-REC
      *
           MOVE  SPA-HOSPNUM                 TO  HKNJA-HOSPNUM
           MOVE  WRK-HKNJANUM                TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC                TO  MCPDATA-REC
           MOVE  "DBSELECT"                  TO  MCP-FUNC
           MOVE  "tbl_hknjainf"     TO  MCP-TABLE
           MOVE  "key2"             TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *
           IF  MCP-RC  =  ZERO
             MOVE  "tbl_hknjainf"     TO  MCP-TABLE
             MOVE  "key2"             TO  MCP-PATHNAME
             PERFORM  925-HKNJAINF-READ-SEC
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
           MOVE  "tbl_hknjainf"     TO  MCP-TABLE
           MOVE  "key2"             TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *
           .
       920-HKNJAINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ情報読込
      *****************************************************************
       925-HKNJAINF-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNJAINF-REC
             MOVE  ZERO         TO  FLG-HKNJAINF
      *
      *    国保でない時は、読み直す
             IF    HKNJA-HKNNUM  NOT  =  "060" AND "067"
               GO               TO  925-HKNJAINF-READ-SEC
             END-IF
           ELSE
             INITIALIZE  HKNJAINF-REC
             MOVE  1            TO  FLG-HKNJAINF
           END-IF
      *
           .
       925-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *  診療情報セット
      *****************************************************************
       930-SRYACT-SET-SEC  SECTION.
      *
             INITIALIZE  SRYACT-REC
             INITIALIZE  FLG-SYOHOKOFU
      *
             MOVE  SPA-HOSPNUM             TO  SRY-HOSPNUM
             MOVE  RECE02-NYUGAIKBN        TO  SRY-NYUGAIKBN
             MOVE  RECE02-PTID             TO  SRY-PTID
             MOVE  RECE02-SRYYM            TO  SRY-SRYYM
             MOVE  SRYACT-REC              TO  MCPDATA-REC
             MOVE  "DBSELECT"              TO  MCP-FUNC
             MOVE  "tbl_sryact"            TO  MCP-TABLE
             MOVE  "key4"                  TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"                 USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_sryact"          TO  MCP-TABLE
               MOVE  "key4"                TO  MCP-PATHNAME
               PERFORM  935-SRYACT-READ-SEC
      *
               PERFORM  UNTIL  FLG-SRYACT  =  1
                 PERFORM  940-SYOHOKOFU-CHK-SEC
      *
                 MOVE  "tbl_sryact"        TO  MCP-TABLE
                 MOVE  "key4"              TO  MCP-PATHNAME
                 PERFORM  935-SRYACT-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SRYACT SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
             MOVE  "tbl_sryact"            TO  MCP-TABLE
             MOVE  "key4"                  TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"                 USING
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
       935-SRYACT-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO      MCP-FUNC
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  SRYACT-REC
             MOVE  ZERO         TO  FLG-SRYACT
           ELSE
             INITIALIZE             SRYACT-REC
             MOVE  1            TO  FLG-SRYACT
           END-IF
      *
           .
       935-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    処方せん交付診療チェック
      *****************************************************************
       940-SYOHOKOFU-CHK-SEC  SECTION.
      *
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL   (IDX3  >     5) OR
                            (SRY-SRYCD(IDX3)  =  SPACE)
             IF  SRY-SRYCD(IDX3)  =  CONS-SYOHOKOFU1 OR CONS-SYOHOKOFU2
                                 OR  CONS-SYOHOKOFU3 OR CONS-SYOHOKOFU4
                                 OR  CONS-SYOHOKOFU5 OR CONS-SYOHOKOFU6
                                 OR  CONS-SYOHOKOFU7 OR CONS-SYOHOKOFU8
                                 OR  CONS-SYOHOKOFU9 OR CONS-SYOHOKOFU10
                                 OR  CONS-SYOHOKOFU11
                                OR  CONS-SYOHOKOFU12 OR CONS-SYOHOKOFU13
                                OR  CONS-SYOHOKOFU14 OR CONS-SYOHOKOFU15
                                OR  CONS-SYOHOKOFU16 OR CONS-SYOHOKOFU17
                                OR  CONS-SYOHOKOFU18 OR CONS-SYOHOKOFU19
                                OR  CONS-SYOHOKOFU20 OR CONS-SYOHOKOFU21
                                OR  CONS-SYOHOKOFU22 OR CONS-SYOHOKOFU23
                                OR  CONS-SYOHOKOFU24 OR CONS-SYOHOKOFU25
                                OR  CONS-SYOHOKOFU26 OR CONS-SYOHOKOFU27
                                OR  CONS-SYOHOKOFU28 OR CONS-SYOHOKOFU29
                 MOVE  1  TO  FLG-SYOHOKOFU
             END-IF
      *      小児かかりつけ診療料
             IF  SRY-SRYCD(IDX3)  =  CONS-SYOHOKOFU30
                                 OR  CONS-SYOHOKOFU31
                 MOVE  1         TO  FLG-SYOHOKOFU
             END-IF
      *      在総診
             PERFORM   VARYING   IDX4   FROM    1   BY   1
                        UNTIL  ( IDX4   >   TBL-ZAITAKU6-MAX )
                 IF  SRY-SRYCD(IDX3)  =  TBL-ZAITAKU6(IDX4)
                     MOVE  1     TO  FLG-SYOHOKOFU
                 END-IF
             END-PERFORM
      *      処方せん無交付加算
             IF  SRY-SRYCD(IDX3)  =  CONS-SYOHOMUKO1
                                 OR  CONS-SYOHOMUKO2
                 MOVE  ZERO      TO  FLG-SYOHOKOFU
             END-IF
           END-PERFORM
      *
           .
       940-SYOHOKOFU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           INITIALIZE                              WRK-TAHOFTN
                                                   WRK-KOHTENSU
                                                   WRK-ETCSHOKUJINISSU
                                                   WRK-ETCSHOKUJIRYOYOHI
                                                   WRK-ETCSHOKUJIFTN
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4   OR
                   SYU-KOH-HKNNUM(IDX) =   SPACE
      *
             IF  (SYU-KOH-HKNNUM(IDX)(1:1)    =   "0" AND
                  SYU-KOH-HKNNUM(IDX)     NOT =   "027")
             OR  (SYU-KOH-HKNNUM(IDX)         =   "972" OR
                  SYU-KOH-HKNNUM(IDX)         =   "974")
      *
      *        他法公費番号取得
               MOVE  SYU-KOH-HKNNUM(IDX)       TO  MF100-TAHO
      *
               IF  RECE02-FTNMONEY(1)    NOT =   ZERO
                 ADD  SYU-KOH-COMPFTN(IDX)     TO  WRK-TAHOFTN
               ELSE
                 ADD  SYU-KOH-COMPFTN-ENTANI(IDX)
                                               TO  WRK-TAHOFTN
               END-IF  
               ADD    SYU-TOTAL-HKNTEN         TO  WRK-KOHTENSU
      *
      *        その他食事療養費（全国公費のみ）
               IF  (SYU-KOH-HKNNUM(IDX)(1:1)    =   "0" AND
                    SYU-KOH-HKNNUM(IDX)     NOT =   "027")
                 ADD  SYU-SYUSKJ-NISSU         TO  WRK-ETCSHOKUJINISSU
                 ADD  SYU-SYUSKJ-RYOYOHI       TO  WRK-ETCSHOKUJIRYOYOHI
                 ADD  SYU-SYUSKJ-FTN           TO  WRK-ETCSHOKUJIFTN
               END-IF
      *
             END-IF
      *
      *      長期チェック
             IF  SYU-KOH-HKNNUM(IDX)  =  "972"
                 MOVE  1                       TO  FLG-TYOKI
                 MOVE  ZERO                    TO  WRK-KOHTENSU
             END-IF
             IF  SYU-KOH-HKNNUM(IDX)  =  "974"
                 MOVE  2                       TO  FLG-TYOKI
                 MOVE  ZERO                    TO  WRK-KOHTENSU
             END-IF
      *
           END-PERFORM
      *
           ADD  WRK-TAHOFTN               TO  MF100-TAHOFTN
           ADD  WRK-KOHTENSU              TO  MF100-TAHOTEN
           ADD  WRK-ETCSHOKUJINISSU       TO  MF100-ETCSHOKUJINISSU
           ADD  WRK-ETCSHOKUJIRYOYOHI     TO  MF100-ETCSHOKUJIRYOYOHI
           ADD  WRK-ETCSHOKUJIFTN         TO  MF100-ETCSHOKUJIFTN
      *
           .
       800-SEIKYU-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納情報セット
      *****************************************************************
       900-SYUNOU-SET-SEC  SECTION.
      *
             INITIALIZE  SYUNOU-REC
             MOVE    SPA-HOSPNUM      TO  SYU-HOSPNUM
             MOVE  RECE02-NYUGAIKBN   TO  SYU-NYUGAIKBN
             MOVE  RECE02-PTID        TO  SYU-PTID
             MOVE  RECE02-SRYYM(1:6)  TO  SYU-SRYYMD(1:6)
             MOVE  "__"               TO  SYU-SRYYMD(7:2)
             MOVE  WRK-KOHNUM         TO  SYU-KOH-HKNNUM(1)
                                          SYU-KOH-HKNNUM(2)
                                          SYU-KOH-HKNNUM(3)
                                          SYU-KOH-HKNNUM(4)
             MOVE  SYUNOU-REC         TO  MCPDATA-REC
             MOVE  "DBSELECT"         TO  MCP-FUNC
             MOVE    "tbl_syunou"       TO  MCP-TABLE
             MOVE    WRK-PATH           TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"        USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_syunou"       TO  MCP-TABLE
               MOVE  WRK-PATH           TO  MCP-PATHNAME
               PERFORM  905-SYUNOU-READ-SEC
      *
               PERFORM  UNTIL  FLG-SYUNOU  =  1
                 IF  SYU-DENPJTIKBN   NOT = "3"
                   IF    SYU-HKNCOMBINUM  =  RECE02-HKNCOMBI(1) OR
                                             RECE02-HKNCOMBI(2) OR
                                             RECE02-HKNCOMBI(3) OR
                                             RECE02-HKNCOMBI(4) OR
                                             RECE02-HKNCOMBI(5) OR
                                             RECE02-HKNCOMBI(6) OR
                                             RECE02-HKNCOMBI(7) OR
                                             RECE02-HKNCOMBI(8) OR
                                             RECE02-HKNCOMBI(9) OR
                                             RECE02-HKNCOMBI(10) 
                     PERFORM  910-HKNCOMBI-READ-SEC
                     IF  COMB-KOHID(1)    =  WRK-KOHID   OR
                         COMB-KOHID(2)    =  WRK-KOHID   OR
                         COMB-KOHID(3)    =  WRK-KOHID   OR
                         COMB-KOHID(4)    =  WRK-KOHID
                       PERFORM  800-SEIKYU-SEC
                     END-IF
                   END-IF
                 END-IF
      *
                 MOVE  "tbl_syunou"     TO  MCP-TABLE
                 MOVE  WRK-PATH         TO  MCP-PATHNAME
                 PERFORM   905-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
             MOVE  "tbl_syunou"         TO  MCP-TABLE
             MOVE  WRK-PATH             TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       905-SYUNOU-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL  "ORCDBMAIN"    USING
                                MCPAREA
                                MCPDATA-REC
                                SPA-AREA
           IF  MCP-RC  =  ZERO
              MOVE  MCPDATA-REC  TO  SYUNOU-REC
              MOVE  ZERO         TO  FLG-SYUNOU
           ELSE
              INITIALIZE  SYUNOU-REC
              MOVE  1  TO  FLG-SYUNOU
           END-IF
      *
           .
       905-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *   保険番号情報セット
      *****************************************************************
       950-HKNNUM-SET-SEC  SECTION.
      *
           INITIALIZE  HKNNUM-REC
           MOVE  SPA-HOSPNUM          TO  HKN-HOSPNUM
           MOVE  WRK-KOHNUM           TO  HKN-HKNNUM
           MOVE  MF100-SRYYM          TO  HKN-TEKSTYMD(1:6)
                                          HKN-TEKEDYMD(1:6)
           MOVE  "99"                 TO  HKN-TEKSTYMD(7:2)
           MOVE  "00"                 TO  HKN-TEKEDYMD(7:2)
           MOVE  HKNNUM-REC               TO  MCPDATA-REC
           MOVE  "DBSELECT"               TO  MCP-FUNC
           MOVE  "tbl_hknnum"       TO  MCP-TABLE
           MOVE  "key5"             TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  "tbl_hknnum"       TO  MCP-TABLE
             MOVE  "key5"             TO  MCP-PATHNAME
             PERFORM  955-HKNNUM-READ-SEC
           ELSE
             DISPLAY  "HKNNUM SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"             TO  MCP-FUNC
           MOVE  "tbl_hknnum"       TO  MCP-TABLE
           MOVE  "key5"             TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *
           .
       950-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者情報読込
      *****************************************************************
       955-HKNNUM-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"  TO  MCP-FUNC
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNNUM-REC
             MOVE  ZERO         TO  FLG-HKNNUM
      *
             IF    HKN-HKNNUM  =  "051"
               INITIALIZE                HKN-TANSEIDONAME
               MOVE    "特定疾患"   TO   HKN-TANSEIDONAME
             END-IF
      *
             IF    HKN-HKNNUM  =  "972" OR "974"
               MOVE    "99"         TO   HKN-HBTNUM
             END-IF
      *
           ELSE
             INITIALIZE  HKNNUM-REC
             MOVE  1  TO  FLG-HKNNUM
           END-IF
      *
           .
       955-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    月末日算出処理
      *****************************************************************
       31012-LASTDAY-GET-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                   TO  LNK-DAY7-IRAI
           MOVE    WRK-SYMD     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                  LNK-DAY7-AREA
           MOVE LNK-DAY7-KOYOMI  TO  WRK-SYMD
           .
       31012-LASTDAY-GET-EXT.
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
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  BTPARA-REC
               MOVE    ZERO                TO  FLG-BTPARA
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           .
       900-BTPARA-READ-N-EXT.
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
           PERFORM 800-SYSKANRI-READ-SEC
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
           PERFORM 800-SYSKANRI-READ-SEC
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
           PERFORM 800-SYSKANRI-READ-SEC
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
           PERFORM 800-SYSKANRI-READ-SEC
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
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC  SECTION.
      *
           MOVE  "DBSELECT"       TO    MCP-FUNC
           MOVE  "tbl_syskanri"   TO    MCP-TABLE
           MOVE  "key10"          TO    MCP-PATHNAME
           CALL  "ORCDBMAIN"      USING
                                  MCPAREA
                                  MCPDATA-REC
                                  SPA-AREA
           IF  MCP-RC  =  ZERO
              MOVE  "DBFETCH"       TO    MCP-FUNC
              MOVE  "tbl_syskanri"  TO    MCP-TABLE
              MOVE  "key10"         TO    MCP-PATHNAME
              CALL  "ORCDBMAIN"     USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
              IF  MCP-RC  =  ZERO
                 MOVE  ZERO  TO  FLG-SYSKANRI
              ELSE
                 MOVE  1  TO  FLG-SYSKANRI
              END-IF
           ELSE
              MOVE  1  TO  FLG-SYSKANRI
           END-IF
      *
           MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
           MOVE  "tbl_syskanri"  TO    MCP-TABLE
           MOVE  "key10"         TO    MCP-PATHNAME
           CALL  "ORCDBMAIN"     USING
                                 MCPAREA
                                 MCPDATA-REC
                                 SPA-AREA
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    プログラムオプション情報読込
      *****************************************************************
       900-PRGOPTION-READ-SEC  SECTION.
      *
           INITIALIZE                      PRGOPTION-REC
           MOVE    SPA-HOSPNUM         TO  PRGOPTION-HOSPNUM
           MOVE    WRK-PRGID           TO  PRGOPTION-PRGID
           MOVE    WRK-KBNCD           TO  PRGOPTION-KBNCD
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PRGOPTION-REC
                   MOVE    ZERO                TO  FLG-PRGOPTION
               ELSE
                   INITIALIZE                      PRGOPTION-REC
                   MOVE    1                   TO  FLG-PRGOPTION
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
           MOVE    "DBCLOSECURSOR"            TO  MCP-FUNC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PRGOPTION-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBOPEN"   TO  MCP-FUNC.
           CALL  "ORCDBMAIN"  USING
                              MCPAREA
                              MCPDATA-REC
                              SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBSTART"  TO  MCP-FUNC.
           CALL  "ORCDBMAIN"  USING
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
       900-DBDISCONNECT-SEC  SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBCOMMIT"      TO  MCP-FUNC.
           CALL  "ORCDBMAIN"  USING
                              MCPAREA
                              MCPDATA-REC
                              SPA-AREA
                               .
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE  "DBDISCONNECT"  TO  MCP-FUNC.
           CALL  "ORCDBMAIN"  USING
                              MCPAREA
                              MCPDATA-REC
                              SPA-AREA
                              .
       900-DBCLOSE-EXT.
           EXIT.
      *
