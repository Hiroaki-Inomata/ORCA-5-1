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
       PROGRAM-ID.             SOKATU2110.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国保・請求書集計処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/05/20    有村          再作成(2.5.0対応)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev 	修正者	日付		内容
      *  01.00.01       内田     06/11/17    帳票様式の変更
      *                                      食事がない場合でも生活療養費を印字
      *  01.00.02       内田     07/08/17    日レセver4.0対応
      *                                      システム管理マスタ「1901」の
      *                                      医療機関情報を参照
      *                                      給付割合が異なっても別帳票としない
      *  01.00.03       内田     08/01/27    一般でも在総診は別帳票とする
      *  01.00.04       内田     08/04/09    SPA-AREA初期化
      *  01.00.05       内田     08/04/23    帳票様式の変更に伴う修正
      *  01.00.06       内田     08/07/09    H20年3月以前分は請求書を分ける
      *  01.00.07       吉川     08/08/07    請求区分が「2 請求済」に
      *                                      ならない不具合を修正
      *  01.00.08       門間     08/11/20    シス管「2007」公費まとめ対応
      *  01.00.09       吉川     09/12/02    シス管「2007」のまとめ公費で
      *                                      6つ目以降がまとまらない不具合を修正
      *  01.00.10       吉川     10/04/05    オンライン返戻対応
      *  01.00.11       吉川     10/10/13    特別療養費国保対応
      *  01.00.12       吉川     11/03/10    open-cobol1.0対応
      *  01.00.13       吉川     13/01/09    プラグイン対応
      *                                      医療機関情報編集対応
      *  01.00.14       吉川     13/07/08    プログラムオプション対応
      *  01.00.15       吉川     14/05/12    前期高齢者２割対応
      *  01.00.16       萩野     14/06/09    在宅の対象とする診療コード追加
      *  01.00.17       吉川     14/10/23    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.18       萩野     14/11/10    返戻時、再印刷画面のタイトル修正
      *  01.00.19       吉川     15/11/17    医療機関編集情報への開設者名追加対応
      *  01.00.20       萩野     16/05/12    在宅の対象とする診療コード追加
      *  01.00.21       門間     17/04/10    医療機関コード変更時、
      *                                      医療機関コード別の作成処理対応
      *  01.00.22       萩野     18/03/05    SITEKBN変更（動作に変更なし）
      *  01.00.23       萩野     18/10/11    修正履歴コメント削除
      *                                      保険番号情報の取得処理を修正
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.

      *    請求書一時ファイル  
           SELECT  SKT2110TMP-FILE ASSIGN    SKT2110TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT2110TMP-KEY
                                   FILE    STATUS  IS  STS-SKT2110.
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
      *    請求書一時ファイル  
       FD  SKT2110TMP-FILE.
           COPY    "CPSKT2110TMP.INC".
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
                                   BY         //SKT2110TP//.
      *
006900*    請求ＤＢ更新用ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEUPD//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *     
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
           COPY    "SKT2110.INC".
           COPY    "SKT2120.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT2110         PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-YUSEN           PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT2110TMP      PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-KOH             PIC 9(01).
           03  FLG-PASS            PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-PRGOPTION       PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDZ1                PIC 9(04).
           03  IDZ2                PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *  パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-HKNJANUM   PIC X(08).
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
           03  WRK-PAYKBN          PIC 9(02).
           03  WRK-RATE            PIC 9(03).
           03  WRK-CNT             PIC 9(02).
           03  WRK-ITBFTN          PIC 9(10).
           03  WRK-KOHNUM          PIC X(03).
           03  WRK-KOHNAME         PIC X(08).
      *
           03  WRK-TENHYO          PIC 99.
           03  WRK-Z2              PIC ZZ.
           03  WRK-Z3              PIC ZZZ.
           03  WRK-Z4              PIC ZZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z6              PIC ZZ,ZZZ.
           03  WRK-Z7              PIC ZZZ,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
      *
           03  WRK-RECEERR         PIC X(200).
      *
      *    文字変換
           03  WRK-MAE-INPUT         PIC X(200).
           03  WRK-OUT-INPUT         PIC X(200).
      *
           03  WRK-PATH            PIC X(50).
           03  WRK-PATH2           PIC X(50).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION      PIC X(100)  OCCURS 100.
           03  WRK-PRGID           PIC X(25).
           03  WRK-KBNCD           PIC X(16).
           03  WRK-YUSEN           PIC X(06).
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
      *    公費ソートエリア
       01  SORT-AREA.
           03  SORT-TBL.
               05  SORT-KOH-TBL  OCCURS  20.
                   07  SORT-KOHNUM            PIC  X(03).
                   07  SORT-KOHNAME             PIC  X(08).
                   07  SORT-K-NYUGAI-TBL  OCCURS  2.
                       09  SORT-KKENSU        PIC  9(06).
                       09  SORT-KNISSU        PIC  9(06).
                       09  SORT-KTENSU        PIC  9(10).
                       09  SORT-KYKZFTN       PIC  9(10).
                       09  SORT-KITBFTN       PIC  9(10).
                   07  SORT-S-KKENSU          PIC  9(06).
                   07  SORT-S-KNISSU          PIC  9(06).
                   07  SORT-S-KKINGAKU        PIC  9(10).
                   07  SORT-S-KFTNGAKU        PIC  9(10).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
           03  CNT-IN              PIC 9(02).
           03  CNT-KOH             PIC 9(02).
      *
      *    公費番号まとめエリア
       01  WRK-2007-AREA.
           03  WRK-2007-TBL        OCCURS  100.
               05  WRK-2007-MEISYO PIC X(44).
               05  WRK-2007-KOH-AREA.
                   07  WRK-2007-KOHNUM
                                   PIC X(03)   OCCURS  10.
           03  WRK-2007-MAX        PIC 9(03).
      *
      *    固定値
       01  CONST-AREA.
      *    在宅総合・在宅末期（在医総）
           03  CONS-ZAITAKU1       PIC X(09) VALUE "114007610".
           03  CONS-ZAITAKU2       PIC X(09) VALUE "114007710".
           03  CONS-ZAITAKU3       PIC X(09) VALUE "114007510".
           03  CONS-ZAITAKU4       PIC X(09) VALUE "114012210".
           03  CONS-ZAITAKU5       PIC X(09) VALUE "114012310".
           03  CONS-ZAITAKU6       PIC X(09) VALUE "114012410".
           03  CONS-ZAITAKU7       PIC X(09) VALUE "114023110".
           03  CONS-ZAITAKU8       PIC X(09) VALUE "114023210".
           03  CONS-ZAITAKU9       PIC X(09) VALUE "114023310".
           03  CONS-ZAITAKU10      PIC X(09) VALUE "114023410".
           03  CONS-ZAITAKU11      PIC X(09) VALUE "114023510".
           03  CONS-ZAITAKU12      PIC X(09) VALUE "114023610".
           03  CONS-ZAITAKU13      PIC X(09) VALUE "114023710".
           03  CONS-ZAITAKU14      PIC X(09) VALUE "114023810".
           03  CONS-ZAITAKU15      PIC X(09) VALUE "114023910".
           03  CONS-ZAITAKU16      PIC X(09) VALUE "114024010".
           03  CONS-ZAITAKU17      PIC X(09) VALUE "114024110".
           03  CONS-ZAITAKU18      PIC X(09) VALUE "114024210".
           03  CONS-ZAITAKU19      PIC X(09) VALUE "114024310".
           03  CONS-ZAITAKU20      PIC X(09) VALUE "114024410".
           03  CONS-ZAITAKU21      PIC X(09) VALUE "114024510".
           03  CONS-ZAITAKU22      PIC X(09) VALUE "114024610".
           03  CONS-ZAITAKU23      PIC X(09) VALUE "114024910".
           03  CONS-ZAITAKU24      PIC X(09) VALUE "114025010".
           03  CONS-ZAITAKU25      PIC X(09) VALUE "114025110".
           03  CONS-ZAITAKU26      PIC X(09) VALUE "114025210".
           03  CONS-ZAITAKU27      PIC X(09) VALUE "114025310".
           03  CONS-ZAITAKU28      PIC X(09) VALUE "114025410".
           03  CONS-ZAITAKU29      PIC X(09) VALUE "114025510".
           03  CONS-ZAITAKU30      PIC X(09) VALUE "114025610".
           03  CONS-ZAITAKU31      PIC X(09) VALUE "114025710".
           03  CONS-ZAITAKU32      PIC X(09) VALUE "114025810".
           03  CONS-ZAITAKU33      PIC X(09) VALUE "114025910".
           03  CONS-ZAITAKU34      PIC X(09) VALUE "114026010".
           03  CONS-ZAITAKU35      PIC X(09) VALUE "114026110".
           03  CONS-ZAITAKU36      PIC X(09) VALUE "114026210".
           03  CONS-ZAITAKU37      PIC X(09) VALUE "114026310".
           03  CONS-ZAITAKU38      PIC X(09) VALUE "114026410".
      *    在がん医総（在支診等）（処方せんあり）
           03  CONS-ZAITAKU39      PIC X(09) VALUE "114007610".
      *    在がん医総（在支診等）（処方せんなし）
           03  CONS-ZAITAKU40      PIC X(09) VALUE "114007710".
      *    在医総・在医総管（ＤＵＭＭＹ）
           03  CONS-ZAITAKU41      PIC X(09) VALUE "099140003".
      *    在がん医総（機能強化した在支診等）（病床あり）（処方せんあり）
           03  CONS-ZAITAKU42      PIC X(09) VALUE "114019510".
      *    在がん医総（機能強化した在支診等）（病床あり）（処方せんなし）
           03  CONS-ZAITAKU43      PIC X(09) VALUE "114019610".
      *    在がん医総（機能強化した在支診等）（病床なし）（処方せんあり）
           03  CONS-ZAITAKU44      PIC X(09) VALUE "114019710".
      *    在がん医総（機能強化した在支診等）（病床なし）（処方せんなし）
           03  CONS-ZAITAKU45      PIC X(09) VALUE "114019810".
      *
      *    在医総管（機能強化在支診等・病床有・難病等月２回以上・１人）
           03  CONS-ZAITAKU46      PIC X(09) VALUE "114030710".
      *    在医総管（機能強化在支診等・病床有・難病等月２回以上・２〜９人）
           03  CONS-ZAITAKU47      PIC X(09) VALUE "114030810".
      *    在医総管（機能強化在支診等・病床有・難病等月２回以上・１０人〜）
           03  CONS-ZAITAKU48      PIC X(09) VALUE "114030910".
      *    在医総管（機能強化在支診等・病床有・月２回以上・１人）
           03  CONS-ZAITAKU49      PIC X(09) VALUE "114031010".
      *    在医総管（機能強化在支診等・病床有・月２回以上・２〜９人）
           03  CONS-ZAITAKU50      PIC X(09) VALUE "114031110".
      *    在医総管（機能強化在支診等・病床有・月２回以上・１０人〜）
           03  CONS-ZAITAKU51      PIC X(09) VALUE "114031210".
      *    在医総管（機能強化在支診等・病床有・月１回・１人）
           03  CONS-ZAITAKU52      PIC X(09) VALUE "114031310".
      *    在医総管（機能強化在支診等・病床有・月１回・２〜９人）
           03  CONS-ZAITAKU53      PIC X(09) VALUE "114031410".
      *    在医総管（機能強化在支診等・病床有・月１回・１０人〜）
           03  CONS-ZAITAKU54      PIC X(09) VALUE "114031510".
      *    在医総管（機能強化在支診等・病床無・難病等月２回以上・１人）
           03  CONS-ZAITAKU55      PIC X(09) VALUE "114031610".
      *    在医総管（機能強化在支診等・病床無・難病等月２回以上・２〜９人）
           03  CONS-ZAITAKU56      PIC X(09) VALUE "114031710".
      *    在医総管（機能強化在支診等・病床無・難病等月２回以上・１０人〜）
           03  CONS-ZAITAKU57      PIC X(09) VALUE "114031810".
      *    在医総管（機能強化在支診等・病床無・月２回以上・１人）
           03  CONS-ZAITAKU58      PIC X(09) VALUE "114031910".
      *    在医総管（機能強化在支診等・病床無・月２回以上・２〜９人）
           03  CONS-ZAITAKU59      PIC X(09) VALUE "114032010".
      *    在医総管（機能強化在支診等・病床無・月２回以上・１０人〜）
           03  CONS-ZAITAKU60      PIC X(09) VALUE "114032110".
      *    在医総管（機能強化在支診等・病床無・月１回・１人）
           03  CONS-ZAITAKU61      PIC X(09) VALUE "114032210".
      *    在医総管（機能強化在支診等・病床無・月１回・２〜９人）
           03  CONS-ZAITAKU62      PIC X(09) VALUE "114032310".
      *    在医総管（機能強化在支診等・病床無・月１回・１０人〜）
           03  CONS-ZAITAKU63      PIC X(09) VALUE "114032410".
      *    在医総管（在支診等・難病等月２回以上・１人）
           03  CONS-ZAITAKU64      PIC X(09) VALUE "114032510".
      *    在医総管（在支診等・難病等月２回以上・２〜９人）
           03  CONS-ZAITAKU65      PIC X(09) VALUE "114032610".
      *    在医総管（在支診等・難病等月２回以上・１０人〜）
           03  CONS-ZAITAKU66      PIC X(09) VALUE "114032710".
      *    在医総管（在支診等・月２回以上・１人）
           03  CONS-ZAITAKU67      PIC X(09) VALUE "114032810".
      *    在医総管（在支診等・月２回以上・２〜９人）
           03  CONS-ZAITAKU68      PIC X(09) VALUE "114032910".
      *    在医総管（在支診等・月２回以上・１０人〜）
           03  CONS-ZAITAKU69      PIC X(09) VALUE "114033010".
      *    在医総管（在支診等・月１回・１人）
           03  CONS-ZAITAKU70      PIC X(09) VALUE "114033110".
      *    在医総管（在支診等・月１回・２〜９人）
           03  CONS-ZAITAKU71      PIC X(09) VALUE "114033210".
      *    在医総管（在支診等・月１回・１０人〜）
           03  CONS-ZAITAKU72      PIC X(09) VALUE "114033310".
      *    在医総管（在支診等以外・難病等月２回以上・１人）
           03  CONS-ZAITAKU73      PIC X(09) VALUE "114033410".
      *    在医総管（在支診等以外・難病等月２回以上・２〜９人）
           03  CONS-ZAITAKU74      PIC X(09) VALUE "114033510".
      *    在医総管（在支診等以外・難病等月２回以上・１０人〜）
           03  CONS-ZAITAKU75      PIC X(09) VALUE "114033610".
      *    在医総管（在支診等以外・月２回以上・１人）
           03  CONS-ZAITAKU76      PIC X(09) VALUE "114033710".
      *    在医総管（在支診等以外・月２回以上・２〜９人）
           03  CONS-ZAITAKU77      PIC X(09) VALUE "114033810".
      *    在医総管（在支診等以外・月２回以上・１０人〜）
           03  CONS-ZAITAKU78      PIC X(09) VALUE "114033910".
      *    在医総管（在支診等以外・月１回・１人）
           03  CONS-ZAITAKU79      PIC X(09) VALUE "114034010".
      *    在医総管（在支診等以外・月１回・２〜９人）
           03  CONS-ZAITAKU80      PIC X(09) VALUE "114034110".
      *    在医総管（在支診等以外・月１回・１０人〜）
           03  CONS-ZAITAKU81      PIC X(09) VALUE "114034210".
      *    施医総管（機能強化在支診等・病床有・難病等月２回以上・１人）
           03  CONS-ZAITAKU82      PIC X(09) VALUE "114035510".
      *    施医総管（機能強化在支診等・病床有・難病等月２回以上・２〜９人）
           03  CONS-ZAITAKU83      PIC X(09) VALUE "114035610".
      *    施医総管（機能強化在支診等・病床有・難病等月２回以上・１０人〜）
           03  CONS-ZAITAKU84      PIC X(09) VALUE "114035710".
      *    施医総管（機能強化在支診等・病床有・月２回以上・１人）
           03  CONS-ZAITAKU85      PIC X(09) VALUE "114035810".
      *    施医総管（機能強化在支診等・病床有・月２回以上・２〜９人）
           03  CONS-ZAITAKU86      PIC X(09) VALUE "114035910".
      *    施医総管（機能強化在支診等・病床有・月２回以上・１０人〜）
           03  CONS-ZAITAKU87      PIC X(09) VALUE "114036010".
      *    施医総管（機能強化在支診等・病床有・月１回・１人）
           03  CONS-ZAITAKU88      PIC X(09) VALUE "114036110".
      *    施医総管（機能強化在支診等・病床有・月１回・２〜９人）
           03  CONS-ZAITAKU89      PIC X(09) VALUE "114036210".
      *    施医総管（機能強化在支診等・病床有・月１回・１０人〜）
           03  CONS-ZAITAKU90      PIC X(09) VALUE "114036310".
      *    施医総管（機能強化在支診等・病床無・難病等月２回以上・１人）
           03  CONS-ZAITAKU91      PIC X(09) VALUE "114036410".
      *    施医総管（機能強化在支診等・病床無・難病等月２回以上・２〜９人）
           03  CONS-ZAITAKU92      PIC X(09) VALUE "114036510".
      *    施医総管（機能強化在支診等・病床無・難病等月２回以上・１０人〜）
           03  CONS-ZAITAKU93      PIC X(09) VALUE "114036610".
      *    施医総管（機能強化在支診等・病床無・月２回以上・１人）
           03  CONS-ZAITAKU94      PIC X(09) VALUE "114036710".
      *    施医総管（機能強化在支診等・病床無・月２回以上・２〜９人）
           03  CONS-ZAITAKU95      PIC X(09) VALUE "114036810".
      *    施医総管（機能強化在支診等・病床無・月２回以上・１０人〜）
           03  CONS-ZAITAKU96      PIC X(09) VALUE "114036910".
      *    施医総管（機能強化在支診等・病床無・月１回・１人）
           03  CONS-ZAITAKU97      PIC X(09) VALUE "114037010".
      *    施医総管（機能強化在支診等・病床無・月１回・２〜９人）
           03  CONS-ZAITAKU98      PIC X(09) VALUE "114037110".
      *    施医総管（機能強化在支診等・病床無・月１回・１０人〜）
           03  CONS-ZAITAKU99      PIC X(09) VALUE "114037210".
      *    施医総管（在支診等・難病等月２回以上・１人）
           03  CONS-ZAITAKU100     PIC X(09) VALUE "114037310".
      *    施医総管（在支診等・難病等月２回以上・２〜９人）
           03  CONS-ZAITAKU101     PIC X(09) VALUE "114037410".
      *    施医総管（在支診等・難病等月２回以上・１０人〜）
           03  CONS-ZAITAKU102     PIC X(09) VALUE "114037510".
      *    施医総管（在支診等・月２回以上・１人）
           03  CONS-ZAITAKU103     PIC X(09) VALUE "114037610".
      *    施医総管（在支診等・月２回以上・２〜９人）
           03  CONS-ZAITAKU104     PIC X(09) VALUE "114037710".
      *    施医総管（在支診等・月２回以上・１０人〜）
           03  CONS-ZAITAKU105     PIC X(09) VALUE "114037810".
      *    施医総管（在支診等・月１回・１人）
           03  CONS-ZAITAKU106     PIC X(09) VALUE "114037910".
      *    施医総管（在支診等・月１回・２〜９人）
           03  CONS-ZAITAKU107     PIC X(09) VALUE "114038010".
      *    施医総管（在支診等・月１回・１０人〜）
           03  CONS-ZAITAKU108     PIC X(09) VALUE "114038110".
      *    施医総管（在支診等以外・難病等月２回以上・１人）
           03  CONS-ZAITAKU109     PIC X(09) VALUE "114038210".
      *    施医総管（在支診等以外・難病等月２回以上・２〜９人）
           03  CONS-ZAITAKU110     PIC X(09) VALUE "114038310".
      *    施医総管（在支診等以外・難病等月２回以上・１０人〜）
           03  CONS-ZAITAKU111     PIC X(09) VALUE "114038410".
      *    施医総管（在支診等以外・月２回以上・１人）
           03  CONS-ZAITAKU112     PIC X(09) VALUE "114038510".
      *    施医総管（在支診等以外・月２回以上・２〜９人）
           03  CONS-ZAITAKU113     PIC X(09) VALUE "114038610".
      *    施医総管（在支診等以外・月２回以上・１０人〜）
           03  CONS-ZAITAKU114     PIC X(09) VALUE "114038710".
      *    施医総管（在支診等以外・月１回・１人）
           03  CONS-ZAITAKU115     PIC X(09) VALUE "114038810".
      *    施医総管（在支診等以外・月１回・２〜９人）
           03  CONS-ZAITAKU116     PIC X(09) VALUE "114038910".
      *    施医総管（在支診等以外・月１回・１０人〜）
           03  CONS-ZAITAKU117     PIC X(09) VALUE "114039010".
      *
           COPY    "CPSHELLTBL.INC".
      *     
           COPY    "COMMON-SPA".
      *
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
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
      *    公費番号まとめ情報
           COPY    "CPSK2007.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      * 
      *    プログラムオプション
       01  PRGOPTION-REC.
           COPY    "CPPRGOPTION.INC".
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
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
           DISPLAY  "START"
      *
           PERFORM 100-DBOPEN-SEC
      *
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
           MOVE    "SOKATU2110"    TO  JOB-PGID
           MOVE    "国保請求書"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "SKT2110"           TO  SKT2110TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT2110TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT2110TP-HOSPNUM
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  SKT2110TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT2110TP-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEERR
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
      *    公費番号まとめ情報
           INITIALIZE                      WRK-2007-AREA
      *
           INITIALIZE                      SYS-2007-REC
           MOVE    "2007"              TO  SYS-2007-KANRICD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-2007-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-2007-STYUKYMD (7:2)
           MOVE    SYS-2007-STYUKYMD   TO  SYS-2007-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-2007-HOSPNUM
           MOVE    SYS-2007-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO            TO  IDX1
           PERFORM         UNTIL   FLG-SYSKANRI   =   1
                           OR      IDX1           >=  100
               MOVE    MCPDATA-REC         TO  SYS-2007-REC
      *
               ADD     1                   TO  IDX1
               MOVE    SYS-2007-MEISYO(1) TO  WRK-2007-MEISYO(IDX1)
               PERFORM VARYING IDX2 FROM    1   BY  1
                       UNTIL   IDX2 >       10
                       OR      SYS-2007-KOHNUM (IDX2)   =   SPACE
                   MOVE    SYS-2007-KOHNUM (IDX2)
                                           TO WRK-2007-KOHNUM(IDX1 IDX2)
               END-PERFORM
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    IDX1                    TO  WRK-2007-MAX
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
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
      *  
           PERFORM 900-SEIKYU-SELECT-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SOKATU2110"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    優先保険者番号
           MOVE    "YUSEN"                   TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           MOVE    WRK-OPTION(1)(1:6)        TO  WRK-YUSEN
           DISPLAY "優先保険者番号="  WRK-YUSEN
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
       200-MAIN-SEC                SECTION.
      *
           OPEN     OUTPUT  SKT2110TMP-FILE
           CLOSE            SKT2110TMP-FILE
           OPEN     I-O     SKT2110TMP-FILE
      *
           MOVE     ZERO    TO      FLG-END
           MOVE     ZERO    TO      FLG-UPD
      *
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   UPD-FILE
           CLOSE   SKT2110TMP-FILE
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
           DISPLAY "*** SOKATU2110 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU2110 END ***"
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *
      *###（地方公費仕様）
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
      *    保険者番号別集計処理
      *****************************************************************
       2001-KOKUHO-SYUKEI-SEC              SECTION.
      * 
      *TMP-FILE キーセット
      *
           INITIALIZE       SKT2110TMP-REC
      *
           IF       RECE10-HKNJANUM(7:2)  =     SPACE
                    MOVE    RECE10-HKNJANUM(1:6)    TO
                            SKT2110TMP-HKNJANUM(1:6)
           ELSE
                    MOVE    RECE10-HKNJANUM(3:6)    TO
                            SKT2110TMP-HKNJANUM(1:6)
           END-IF
      *
            MOVE  999  TO  SKT2110TMP-KYURATE
      *
      *    一般／退職
           IF  RECE10-HKNNUM  =  "067" OR "069"
             MOVE  2  TO  SKT2110TMP-SYUBETU
           ELSE
             MOVE  1  TO  SKT2110TMP-SYUBETU
           END-IF
      *
           IF    WRK-YUSEN         =    SKT2110TMP-HKNJANUM
             MOVE    9    TO    SKT2110TMP-SYUTURYOKU
           ELSE
             IF    RECE10-PREFKBN    =    "1"
                 IF    SKT2110TMP-HKNJANUM(3:1)  =  "3"
                   MOVE   2    TO    SKT2110TMP-SYUTURYOKU
                 ELSE
                   MOVE   3    TO    SKT2110TMP-SYUTURYOKU
                 END-IF
             ELSE
                 MOVE     1    TO    SKT2110TMP-SYUTURYOKU
             END-IF
           END-IF
      *
      *    在宅
           MOVE   RECE10-ZAITAKU    TO   SKT2110TMP-ZAITAKU
      *
      *    在宅チェック
           PERFORM  960-SRYACT-SET-SEC
      *    平成２０年３月以前分は別帳票
           IF  RECE10-SRYYM  <  WRK-CONS-SRYYM-200804
             MOVE  1                TO  SKT2110TMP-SRYYMFLG
           END-IF
      *
           PERFORM  900-SKTTMP-READ-SEC
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
      *    老人保健か
           IF  RECE10-RJNHKNNUM  NOT  =  SPACE
             IF    RECE10-KYURATE  =  9
               MOVE    1    TO    IDX1
             ELSE
               MOVE    2    TO    IDX1
             END-IF
      *
             PERFORM  2101-RJN-SYUKEI-SEC
      *    公費併用
             IF  RECE10-KOHNUM(1)  NOT =  SPACE
               PERFORM  221-KOH-SYUKEI-SEC
             END-IF
           ELSE
      *    一般／退職者
             IF  RECE10-HKNNUM  =  "060" OR "068"
      *    年齢チェック
               IF  RECE10-AGE  >=  70
                   EVALUATE  RECE10-KYURATE
                     WHEN  9
                       MOVE    1     TO    IDX1
                     WHEN  8
                       IF  RECE10-RECESYUBETU(4:1)  =  "7" OR "8"
                           MOVE    1    TO    IDX1
                       ELSE
                           MOVE    2    TO    IDX1
                       END-IF
                     WHEN  7
                       MOVE    2     TO    IDX1
                   END-EVALUATE
               ELSE
                   IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                   AND  (RECE10-FLG (1)   =  1)
                       MOVE  4     TO  IDX1
                   ELSE
                       IF  RECE10-AGE  >=  3
                           MOVE  3     TO  IDX1
                       ELSE
                           MOVE  4     TO  IDX1
                       END-IF
                   END-IF
               END-IF
             ELSE
               IF    RECE10-AGE   >= 70
                 IF    RECE10-KYURATE  =  9
                   MOVE      2    TO    IDX1
                 ELSE
                   MOVE      3    TO    IDX1
                 END-IF
               ELSE
                 IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                 AND  (RECE10-FLG (1)   =  1)
                     MOVE  5     TO  IDX1
                 ELSE
                   IF    RECE10-AGE  >=  3
                     IF    RECE10-HONKZKKBN  =  "1"
                       MOVE    1    TO    IDX1
                     ELSE
                       MOVE    4    TO    IDX1
                     END-IF
                   ELSE
                     MOVE      5    TO    IDX1
                   END-IF
                 END-IF
               END-IF
             END-IF
      *
             PERFORM  2102-IPA-SYUKEI-SEC
      *    公費併用
             IF  RECE10-KOHNUM(1)  NOT =  SPACE
               PERFORM  220-KOH-SYUKEI-SEC
             END-IF
           END-IF
      *
           IF  FLG-SKT2110TMP  =  1
               WRITE    SKT2110TMP-REC
           ELSE
               REWRITE  SKT2110TMP-REC
           END-IF
      *
           MOVE    1    TO  FLG-UPD
      *
           .
       210-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・老人分集計処理
      *****************************************************************
       2101-RJN-SYUKEI-SEC         SECTION.
      *
      *    総件数
           ADD   1                   TO  SKT2110TMP-RGKENSU
      *    療養の給付：件数
           ADD   1                   TO  SKT2110TMP-RKENSU (IDX1 IDX2)
      *    療養の給付：診療実日数
           ADD   RECE10-JNISSU (1)   TO  SKT2110TMP-RNISSU (IDX1 IDX2)
      *    療養の給付：点数
           ADD   RECE10-TOTALTEN(1)  TO  SKT2110TMP-RTENSU (IDX1 IDX2)
      *    療養の給付：一部負担金
           ADD   RECE10-FTNMONEY(1)  TO  SKT2110TMP-RITBFTN(IDX1 IDX2)
      *
           IF  RECE10-SHOKUJINISSU(1)  >  0
      *    食事療養：件数
             ADD   1                 TO  SKT2110TMP-S-RKENSU (IDX1)
           END-IF
      *    食事療養：日数
           ADD   RECE10-SHOKUJINISSU (1)
                                     TO  SKT2110TMP-S-RNISSU (IDX1)
      *    食事療養：金額
           ADD   RECE10-SHOKUJIRYOYOHI (1)
                                     TO  SKT2110TMP-S-RKINGAKU(IDX1)
      *    食事療養：標準負担額
           ADD   RECE10-SHOKUJIFTN (1)
                                     TO  SKT2110TMP-S-RFTNGAKU(IDX1)
      *    長期
           IF  RECE10-ETC-KOHNUM(1) = "972" OR "974" OR
               RECE10-ETC-KOHNUM(2) = "972" OR "974" OR
               RECE10-ETC-KOHNUM(3) = "972" OR "974" OR
               RECE10-ETC-KOHNUM(4) = "972" OR "974"
             ADD  1  TO  SKT2110TMP-RTKENSU (IDX2)
           END-IF
      *
           MOVE    1               TO  FLG-UPD
           .
       2101-RJN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・一般・退職分集計処理
      *****************************************************************
       2102-IPA-SYUKEI-SEC         SECTION.
      *
      *    総件数
           ADD   1                   TO  SKT2110TMP-GKENSU
      *    療養の給付：件数
           ADD   1                   TO  SKT2110TMP-KENSU (IDX1 IDX2)
      *    療養の給付：診療実日数
           ADD   RECE10-JNISSU (1)   TO  SKT2110TMP-NISSU (IDX1 IDX2)
      *    療養の給付：点数
           ADD   RECE10-TOTALTEN(1)  TO  SKT2110TMP-TENSU (IDX1 IDX2)
      *    療養の給付：一部負担金
           ADD   RECE10-FTNMONEY(1)  TO  SKT2110TMP-ITBFTN(IDX1 IDX2)
      * 
           IF  RECE10-SHOKUJINISSU(1)  >  0
      *    食事療養：件数
             ADD   1                 TO  SKT2110TMP-S-KENSU (IDX1)
           END-IF
      *    食事療養：日数
           ADD   RECE10-SHOKUJINISSU (1)
                                     TO  SKT2110TMP-S-NISSU (IDX1)
      *    食事療養：金額
           ADD   RECE10-SHOKUJIRYOYOHI (1)
                                     TO  SKT2110TMP-S-KINGAKU(IDX1)
      *    食事療養：標準負担額
           ADD   RECE10-SHOKUJIFTN (1)
                                     TO  SKT2110TMP-S-FTNGAKU(IDX1)
      *    長期
           IF  RECE10-ETC-KOHNUM(1) = "972" OR "974" OR
               RECE10-ETC-KOHNUM(2) = "972" OR "974" OR
               RECE10-ETC-KOHNUM(3) = "972" OR "974" OR
               RECE10-ETC-KOHNUM(4) = "972" OR "974"
             ADD  1  TO  SKT2110TMP-TKENSU (IDX2)
           END-IF
      *
           MOVE    1               TO  FLG-UPD
           .
       2102-IPA-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担分集計処理
      *****************************************************************
       220-KOH-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDX3     FROM    1   BY  1
                   UNTIL   IDX3     >       4
                   OR  RECE10-KOHNUM (IDX3) =   SPACE
      *
      *      ５１、９１は１つにまとめる
             IF    RECE10-KOHNUM(IDX3)  =  "091"
               MOVE    "051"       TO    RECE10-KOHNUM(IDX3)
             END-IF
      *
      *      公費番号まとめ情報があるとき
               IF  WRK-2007-MAX  >  ZERO
                 MOVE  SPACE     TO  WRK-KOHNAME
                 PERFORM VARYING IDZ1    FROM    1   BY  1
                         UNTIL   IDZ1    >       WRK-2007-MAX
                   PERFORM VARYING IDZ2  FROM    1   BY  1
                           UNTIL   IDZ2  >       10
                           OR WRK-2007-KOHNUM(IDZ1 IDZ2)  =  SPACE
                     IF  RECE10-KOHNUM(IDX3)  
                                   =   WRK-2007-KOHNUM(IDZ1 IDZ2)
                       MOVE  WRK-2007-KOHNUM(IDZ1 1)
                                   TO  RECE10-KOHNUM (IDX3)
                       MOVE  WRK-2007-MEISYO (IDZ1)
                                   TO  WRK-KOHNAME
                       MOVE  10            TO  IDZ2
                       MOVE  WRK-2007-MAX  TO  IDZ1
                     END-IF
                   END-PERFORM
                 END-PERFORM
               END-IF
      *
      *    公費判別
      *
      *    全国共通公費のみを対象にする
             IF    RECE10-KOHNUM(IDX3)(1:1)  =  "0"
               PERFORM  VARYING  IDX4  FROM  1  BY  1
                        UNTIL    IDX4  >     20
                        OR       SKT2110TMP-KOHNUM (IDX4) = SPACE
                 IF  RECE10-KOHNUM (IDX3)  =  SKT2110TMP-KOHNUM (IDX4)
                     MOVE  IDX4  TO  CNT-KOH
                     MOVE  20    TO  IDX4
                 END-IF
               END-PERFORM
      *
      *    該当公費がなかった場合
               IF  IDX4  < 20
                 MOVE  IDX4  TO  CNT-KOH
               END-IF
      *    公費名
               MOVE  RECE10-KOHNUM(IDX3)
                                    TO  SKT2110TMP-KOHNUM(CNT-KOH)
      *    公費名称
               MOVE  WRK-KOHNAME    TO  SKT2110TMP-KOHNAME(CNT-KOH)
      *
      *    療養の給付：件数
               ADD  1               TO SKT2110TMP-KKENSU(CNT-KOH IDX2)
      *    療養の給付:診療実日数
               ADD  RECE10-JNISSU (IDX3 + 1) 
                                   TO SKT2110TMP-KNISSU(CNT-KOH IDX2)
      *    療養の給付：点数
               ADD  RECE10-TOTALTEN(IDX3 + 1)
                                    TO SKT2110TMP-KTENSU(CNT-KOH IDX2)
      *    療養の給付：一部負担金
               ADD  RECE10-FTNMONEY (IDX3 + 1)
                                   TO SKT2110TMP-KITBFTN(CNT-KOH IDX2)
      *    食事療養費追加
               IF  RECE10-SHOKUJINISSU(IDX3 + 1)  >  0
      *    食事療養:件数
                 ADD   1  TO   SKT2110TMP-S-KKENSU(CNT-KOH)
               END-IF
      *    食事療養:日数
                 ADD  RECE10-SHOKUJINISSU(IDX3 + 1)    TO
                               SKT2110TMP-S-KNISSU(CNT-KOH)
      *    食事療養：金額
                 ADD  RECE10-SHOKUJIRYOYOHI(IDX3 + 1)  TO
                               SKT2110TMP-S-KKINGAKU(CNT-KOH)
      *    食事療養：標準負担額
                 ADD  RECE10-SHOKUJIFTN(IDX3 + 1)      TO
                               SKT2110TMP-S-KFTNGAKU(CNT-KOH)
               END-IF
           END-PERFORM
      *
           .
       220-KOH-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人公費負担分集計処理
      *****************************************************************
       221-KOH-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDX3     FROM    1   BY  1
                   UNTIL   IDX3     >       4
                   OR  RECE10-KOHNUM (IDX3) =   SPACE
      *
      *      ５１、９１は１つにまとめる
             IF    RECE10-KOHNUM(IDX3)  =  "091"
               MOVE    "051"       TO    RECE10-KOHNUM(IDX3)
             END-IF
      *
      *      公費番号まとめ情報があるとき
               IF  WRK-2007-MAX  >  ZERO
                 MOVE  SPACE     TO  WRK-KOHNAME
                 PERFORM VARYING IDZ1    FROM    1   BY  1
                         UNTIL   IDZ1    >       WRK-2007-MAX
                   PERFORM VARYING IDZ2  FROM    1   BY  1
                           UNTIL   IDZ2  >       10
                           OR WRK-2007-KOHNUM(IDZ1 IDZ2)  =  SPACE
                     IF  RECE10-KOHNUM(IDX3)  
                                   =   WRK-2007-KOHNUM(IDZ1 IDZ2)
                       MOVE  WRK-2007-KOHNUM(IDZ1 1)
                                   TO  RECE10-KOHNUM (IDX3)
                       MOVE  WRK-2007-MEISYO (IDZ1)
                                   TO  WRK-KOHNAME
                       MOVE  10            TO  IDZ2
                       MOVE  WRK-2007-MAX  TO  IDZ1
                     END-IF
                   END-PERFORM
                 END-PERFORM
               END-IF
      *
      *    全国共通公費のみを対象にする
             IF    RECE10-KOHNUM(IDX3)(1:1)  =  "0"
      *
      *        公費判別
               PERFORM  VARYING  IDX4  FROM  1  BY  1
                        UNTIL    IDX4  >     20
                        OR       SKT2110TMP-RKOHNUM (IDX4) = SPACE
                 IF  RECE10-KOHNUM (IDX3)  =  SKT2110TMP-RKOHNUM (IDX4)
                     MOVE  IDX4  TO  CNT-KOH
                     MOVE  20    TO  IDX4
                 END-IF
               END-PERFORM
      *
      *    該当公費がなかった場合
               IF  IDX4  < 20
                 MOVE  IDX4  TO  CNT-KOH
               END-IF
      *    公費名
               MOVE  RECE10-KOHNUM(IDX3) TO  SKT2110TMP-RKOHNUM(CNT-KOH)
      *
      *    公費名称
             MOVE  WRK-KOHNAME      TO  SKT2110TMP-RKOHNAME(CNT-KOH)
      *    療養の給付：件数
               ADD  1           TO SKT2110TMP-RKKENSU(CNT-KOH IDX2)
      *    療養の給付:診療実日数
               ADD  RECE10-JNISSU (IDX3 + 1)
                                   TO SKT2110TMP-RKNISSU(CNT-KOH IDX2)
      *    療養の給付：点数
               ADD  RECE10-TOTALTEN(IDX3 + 1) 
                                   TO SKT2110TMP-RKTENSU(CNT-KOH IDX2)
      *
      *    療養の給付：一部負担金
               ADD  RECE10-FTNMONEY (IDX3 + 1)
                                     TO
                                SKT2110TMP-RKITBFTN(CNT-KOH IDX2)
      *    食事療養費追加
               IF  RECE10-SHOKUJINISSU(IDX3 + 1)  >  0
      *    食事療養:件数
                 ADD   1  TO   SKT2110TMP-S-RKKENSU(CNT-KOH)
               END-IF
      *    食事療養:日数
               ADD  RECE10-SHOKUJINISSU(IDX3 + 1)    TO
                               SKT2110TMP-S-RKNISSU(CNT-KOH)
      *    食事療養：金額
               ADD  RECE10-SHOKUJIRYOYOHI(IDX3 + 1)  TO
                               SKT2110TMP-S-RKKINGAKU(CNT-KOH)
      *    食事療養：標準負担額
               ADD  RECE10-SHOKUJIFTN(IDX3 + 1)      TO
                               SKT2110TMP-S-RKFTNGAKU(CNT-KOH)
               END-IF
           END-PERFORM
      *
           .
       221-KOH-SYUKEI-EXT.
           EXIT.
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
           OPEN     INPUT       SKT2110TMP-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
           MOVE     ZERO        TO       FLG-PASS
           MOVE     1           TO       WRK-CNT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =   1
      *
      *    公費ソート処理
              PERFORM  KOH-SORT-SEC
      *    一般の患者はいるか
             IF  SKT2110TMP-GKENSU  >  0
      *    公費が残っているか
               PERFORM  UNTIL  SKT2110TMP-KOHNUM(WRK-CNT) = SPACE
                        AND    FLG-PASS  =  1
      *    帳票編集<見出し>処理
                 PERFORM 311-HEAD-HEN-SEC
      *    帳票編集<明細>処理
                 PERFORM 321-BODY-HEN-SEC
      *    帳票印刷処理
                 PERFORM 331-PRINT-OUT-SEC
                 MOVE  1  TO  FLG-PASS
               END-PERFORM
             END-IF
             MOVE  ZERO  TO  FLG-PASS
             MOVE  1     TO  WRK-CNT
      *    老人の患者はいるか
             IF  SKT2110TMP-RGKENSU  >  0
      *    公費が残っているか
               PERFORM  UNTIL  SKT2110TMP-RKOHNUM(WRK-CNT) = SPACE
                       AND    FLG-PASS  =  1
      *    帳票編集<見出し>処理
                 PERFORM 313-HEAD-HEN-SEC
      *    帳票編集<明細>処理
                 PERFORM 323-BODY-HEN-SEC
      *    帳票印刷処理
                 PERFORM 333-PRINT-OUT-SEC
                 MOVE  1  TO  FLG-PASS
               END-PERFORM
             END-IF
             MOVE  ZERO  TO  FLG-PASS
             MOVE  1     TO  WRK-CNT
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE    SKT2110TMP-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理（一般）
      *****************************************************************
       311-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT2110
      *    NO
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT2110-NO
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT2110-SRYYM
      *    請求年月日
           MOVE    WRK-SEIYMDWH          TO  SKT2110-SEIYMD
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT2110-ADRS
           INSPECT SKT2110-ADRS          REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT2110-HOSPNAME
           INSPECT SKT2110-HOSPNAME      REPLACING ALL "  " BY "　"
      *    開設者名
           MOVE    WRK-KAISETUNAME       TO  SKT2110-KAISETUNAME
      *    電話番号
           MOVE    WRK-HOSPTEL           TO  SKT2110-TEL
      *    保険者番号
           MOVE    SKT2110TMP-HKNJANUM   TO   WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT         TO  SKT2110-HKNJANUM
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD       TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT         TO  SKT2110-HOSPCD
      *    点数表区分
           MOVE    SYS-1001-TENHYOKBN    TO  WRK-TENHYO
           MOVE    WRK-TENHYO            TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT         TO  SKT2110-TENHYOKBN
      *
      *    診療科
           MOVE    SPACE                 TO  SKT2110-SRYKA
      *    給付割合
           EVALUATE    SKT2110TMP-KYURATE
               WHEN    7
                   MOVE    "○"        TO  SKT2110-KYURATE(1)
               WHEN    8
                   MOVE    "○"        TO  SKT2110-KYURATE(2)
               WHEN    9
                   MOVE    "○"        TO  SKT2110-KYURATE(3)
               WHEN    0
                   MOVE    "○"        TO  SKT2110-KYURATE(4)
           END-EVALUATE
      *    一般／退職
           IF  SKT2110TMP-SYUBETU  =  "1"
             MOVE  "○"  TO  SKT2110-HKNSYUBETU(1)
           ELSE
             MOVE  "○"  TO  SKT2110-HKNSYUBETU(2)
           END-IF
      *
           IF     SKT2110TMP-ZAITAKU  =  "1"
             MOVE  "在"    TO    SKT2110-ZAI1
             MOVE  "◯"    TO    SKT2110-ZAI2
           END-IF
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"         TO  SKT2110-TOKUBETU
           END-IF
      *
           .
       311-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理（一般）
      *****************************************************************
       321-BODY-HEN-SEC      SECTION.
      *
      *    公費の為の２枚目以上か
           IF  WRK-CNT  =  1
      *    編集処理
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     5
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     2
      *    療養の給付：件数
                 MOVE  SKT2110TMP-KENSU (IDX1 IDX2)   TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT2110-KENSU(IDX1 IDX2)
      *    療養の給付：日数
                 MOVE  SKT2110TMP-NISSU (IDX1 IDX2)   TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT2110-NISSU(IDX1 IDX2)
      *    療養の給付：点数
                 MOVE  SKT2110TMP-TENSU (IDX1 IDX2)   TO  WRK-Z9
                 MOVE  WRK-Z9   TO  SKT2110-TENSU(IDX1 IDX2)
      *    療養の給付：一部負担金
                 MOVE  SKT2110TMP-ITBFTN (IDX1 IDX2)  TO  WRK-Z9
                 MOVE  WRK-Z9   TO  SKT2110-ITBFTN(IDX1 IDX2)
               END-PERFORM
      *    食事療養：件数
               MOVE  SKT2110TMP-S-KENSU (IDX1)        TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT2110-S-KENSU(IDX1)
      *    食事療養：日数
               MOVE  SKT2110TMP-S-NISSU (IDX1)        TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT2110-S-NISSU(IDX1)
      *    食事療養：金額
               MOVE  SKT2110TMP-S-KINGAKU (IDX1)      TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT2110-S-KINGAKU(IDX1)
      *    食事療養：標準負担額
               MOVE  SKT2110TMP-S-FTNGAKU (IDX1)      TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT2110-S-FTNGAKU(IDX1)
             END-PERFORM
      *    長期
             MOVE  SKT2110TMP-TKENSU (1)              TO  WRK-Z2
             MOVE  WRK-Z2     TO  SKT2110-TKENSU(1)
             MOVE  SKT2110TMP-TKENSU (2)              TO  WRK-Z2
             MOVE  WRK-Z2     TO  SKT2110-TKENSU(2)
           END-IF
      *
      **    公費の印字
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     3
                    OR  WRK-CNT    >     20
                    OR  SKT2110TMP-KOHNUM(WRK-CNT) = SPACE
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     2
      *    療養の給付：件数
               MOVE  SKT2110TMP-KKENSU (WRK-CNT IDX2)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT2110-KKENSU(IDX3 IDX2)
      *    療養の給付：日数
               MOVE  SKT2110TMP-KNISSU (WRK-CNT IDX2)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT2110-KNISSU(IDX3 IDX2)
      *    療養の給付：点数
               MOVE  SKT2110TMP-KTENSU (WRK-CNT IDX2)   TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT2110-KTENSU(IDX3 IDX2)
      *    療養の給付：一部負担金
               MOVE  SKT2110TMP-KITBFTN (WRK-CNT IDX2)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT2110-KITBFTN(IDX3 IDX2)
             END-PERFORM
      *    食事療養：件数
             MOVE  SKT2110TMP-S-KKENSU (WRK-CNT)        TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT2110-S-KKENSU(IDX3)
      *    食事療養：日数
             MOVE  SKT2110TMP-S-KNISSU (WRK-CNT)        TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT2110-S-KNISSU(IDX3)
      *    食事療養：金額
             MOVE  SKT2110TMP-S-KKINGAKU (WRK-CNT)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT2110-S-KKINGAKU(IDX3)
      *    食事療養：標準負担額
             MOVE  SKT2110TMP-S-KFTNGAKU (WRK-CNT)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT2110-S-KFTNGAKU(IDX3)
      *    公費名
             MOVE  SKT2110TMP-KOHNUM (WRK-CNT)  TO  WRK-KOHNUM
             PERFORM  950-HKNNUM-SET-SEC
             MOVE  HKN-TANSEIDONAME(1:8)   TO  SKT2110-KOHNAME (IDX3)
      *
             IF  SKT2110TMP-KOHNAME (WRK-CNT)  NOT =  SPACE
               MOVE  SKT2110TMP-KOHNAME (WRK-CNT)
                                        TO  SKT2110-KOHNAME(IDX3)
             ELSE
               MOVE  HKN-TANSEIDONAME   TO  SKT2110-KOHNAME(IDX3)
             END-IF
      *
             ADD  1  TO  WRK-CNT
           END-PERFORM
      *
           .
       321-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理（一般）
      *****************************************************************
       331-PRINT-OUT-SEC                SECTION.
      *
           DISPLAY  "PRINT1"
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
           MOVE    "SKT2110.red"       TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "国保請求書"                TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "国保請求書［返戻分］"      TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "国保請求書［特別療養費］"  TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "国保請求書［返戻分］［特別療養費］" TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT2110             TO  SPRT-PRTDATA
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
       331-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理（老人）
      *****************************************************************
       313-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT2120
      *    NO
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT2120-NO
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT2120-SRYYM
      *    請求年月日
           MOVE    WRK-SEIYMDWH          TO  SKT2120-SEIYMD
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT2120-ADRS
           INSPECT SKT2120-ADRS          REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT2120-HOSPNAME
           INSPECT SKT2120-HOSPNAME      REPLACING ALL "  " BY "　"
      *    開設者名
           MOVE    WRK-KAISETUNAME       TO  SKT2120-KAISETUNAME
      *    電話番号
           MOVE    WRK-HOSPTEL           TO  SKT2120-TEL
      *    保険者番号
           MOVE    SKT2110TMP-HKNJANUM   TO   WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT         TO  SKT2120-HKNJANUM
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD       TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT         TO  SKT2120-HOSPCD
      *    点数表区分
           MOVE    SYS-1001-TENHYOKBN    TO  WRK-TENHYO
           MOVE    WRK-TENHYO            TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT         TO  SKT2110-TENHYOKBN
      *
      *    診療科
           MOVE    SPACE                 TO  SKT2120-SRYKA
      *    給付割合
           EVALUATE    SKT2110TMP-KYURATE
               WHEN    7
                   MOVE    "○"        TO  SKT2120-KYURATE(1)
               WHEN    8
                   MOVE    "○"        TO  SKT2120-KYURATE(2)
               WHEN    9
                   MOVE    "○"        TO  SKT2120-KYURATE(3)
               WHEN    0
                   MOVE    "○"        TO  SKT2120-KYURATE(4)
           END-EVALUATE
      *
      *
           IF     SKT2110TMP-ZAITAKU  =  "1"
             MOVE  "在"    TO    SKT2120-ZAI1
             MOVE  "◯"    TO    SKT2120-ZAI2
           END-IF
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"         TO  SKT2120-TOKUBETU
           END-IF
      *
           .
       313-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理（老人）
      *****************************************************************
       323-BODY-HEN-SEC      SECTION.
      *
      *    公費の為の２枚目以上か
           IF  WRK-CNT  =  1
      *    編集処理
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     2
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     2
      *    療養の給付：件数
                 MOVE  SKT2110TMP-RKENSU (IDX1 IDX2)   TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT2120-KENSU(IDX1 IDX2)
      *    療養の給付：日数
                 MOVE  SKT2110TMP-RNISSU (IDX1 IDX2)   TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT2120-NISSU(IDX1 IDX2)
      *    療養の給付：点数
                 MOVE  SKT2110TMP-RTENSU (IDX1 IDX2)   TO  WRK-Z9
                 MOVE  WRK-Z9   TO  SKT2120-TENSU(IDX1 IDX2)
      *    療養の給付：一部負担金
                 MOVE  SKT2110TMP-RITBFTN (IDX1 IDX2)  TO  WRK-Z9
                 MOVE  WRK-Z9   TO  SKT2120-ITBFTN(IDX1 IDX2)
               END-PERFORM
      *    食事療養：件数
               MOVE  SKT2110TMP-S-RKENSU (IDX1)        TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT2120-S-KENSU(IDX1)
      *    食事療養：日数
               MOVE  SKT2110TMP-S-RNISSU (IDX1)        TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT2120-S-NISSU(IDX1)
      *    食事療養：金額
               MOVE  SKT2110TMP-S-RKINGAKU (IDX1)      TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT2120-S-KINGAKU(IDX1)
      *    食事療養：標準負担額
               MOVE  SKT2110TMP-S-RFTNGAKU (IDX1)      TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT2120-S-FTNGAKU(IDX1)
             END-PERFORM
      *    長期
             MOVE  SKT2110TMP-RTKENSU (1)              TO  WRK-Z2
             MOVE  WRK-Z2     TO  SKT2110-TKENSU(1)
             MOVE  SKT2110TMP-RTKENSU (2)              TO  WRK-Z2
             MOVE  WRK-Z2     TO  SKT2110-TKENSU(2)
           END-IF
      *
      *    公費の印字
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     3
                    OR  WRK-CNT    >     20
                    OR  SKT2110TMP-RKOHNUM(WRK-CNT) = SPACE
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     2
      *    療養の給付：件数
               MOVE  SKT2110TMP-RKKENSU (WRK-CNT IDX2)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT2120-KKENSU(IDX3 IDX2)
      *    療養の給付：日数
               MOVE  SKT2110TMP-RKNISSU (WRK-CNT IDX2)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT2120-KNISSU(IDX3 IDX2)
      *    療養の給付：点数
               MOVE  SKT2110TMP-RKTENSU (WRK-CNT IDX2)   TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT2120-KTENSU(IDX3 IDX2)
      *    療養の給付：一部負担金
               MOVE  SKT2110TMP-RKITBFTN (WRK-CNT IDX2)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT2120-KITBFTN(IDX3 IDX2)
             END-PERFORM
      *    食事療養：件数
             MOVE  SKT2110TMP-S-RKKENSU (WRK-CNT)        TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT2120-S-KKENSU(IDX3)
      *    食事療養：日数
             MOVE  SKT2110TMP-S-RKNISSU (WRK-CNT)        TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT2120-S-KNISSU(IDX3)
      *    食事療養：金額
             MOVE  SKT2110TMP-S-RKKINGAKU (WRK-CNT)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT2120-S-KKINGAKU(IDX3)
      *    食事療養：標準負担額
             MOVE  SKT2110TMP-S-RKFTNGAKU (WRK-CNT)      TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT2120-S-KFTNGAKU(IDX3)
      *    公費名
             MOVE  SKT2110TMP-RKOHNUM (WRK-CNT)  TO  WRK-KOHNUM
             PERFORM  950-HKNNUM-SET-SEC
             MOVE  HKN-TANSEIDONAME(1:8)   TO  SKT2120-KOHNAME (IDX3)
      *
             IF  SKT2110TMP-RKOHNAME (WRK-CNT)  NOT =  SPACE
               MOVE  SKT2110TMP-RKOHNAME (WRK-CNT)
                                        TO  SKT2120-KOHNAME(IDX3)
             ELSE
               MOVE  HKN-TANSEIDONAME   TO  SKT2120-KOHNAME(IDX3)
             END-IF
      *
             ADD  1  TO  WRK-CNT
           END-PERFORM
      *
           .
       323-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理（老人）
      *****************************************************************
       333-PRINT-OUT-SEC                SECTION.
      *
           DISPLAY  "PRINT2"
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
           MOVE    "SKT2120.red"       TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "国保請求書"                TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "国保請求書［返戻分］"      TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "国保請求書［特別療養費］"  TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "国保請求書［返戻分］［特別療養費］" TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT2120             TO  SPRT-PRTDATA
           DISPLAY "老人印刷"
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
       333-PRINT-OUT-EXT.
           EXIT.
057400*
      *****************************************************************
      *    公費ソート処理
      *****************************************************************
       KOH-SORT-SEC          SECTION.
      *
      *
      *    一般公費分ソート
           INITIALIZE  SORT-AREA
      *    公費数を取得
           MOVE    ZERO    TO    IDX-MAX
                                 IDX2 
           MOVE    SPACE   TO    WRK-KOHNUM
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >  20
             IF  SKT2110TMP-KOHNUM(IDX1)  =  SPACE
               MOVE  20    TO    IDX1
             ELSE
               IF    WRK-KOHNUM               =   SPACE   OR
                     SKT2110TMP-KOHNUM(IDX1)  <   WRK-KOHNUM
                 MOVE     IDX1                TO  IDX2
                 MOVE     SKT2110TMP-KOHNUM(IDX1) TO  WRK-KOHNUM
               END-IF
               ADD   1     TO    IDX-MAX
             END-IF
           END-PERFORM
           IF     IDX2     >     ZERO
             MOVE    SKT2110TMP-KOH-TBL(IDX2) TO  SORT-KOH-TBL(1)
           END-IF
      
           DISPLAY "一般ソート前１" SKT2110TMP-KOHNUM(1)
           DISPLAY "一般ソート前２" SKT2110TMP-KOHNUM(2)
           DISPLAY "一般ソート前３" SKT2110TMP-KOHNUM(3)
      *
           IF      IDX-MAX    >     ZERO
             MOVE    2    TO    IDX1
             MOVE    1    TO    IDX2
             MOVE    1    TO    IDX3
             MOVE   "999"   TO    WRK-KOHNUM
             PERFORM    UNTIL    IDX1  >    IDX-MAX
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >    IDX-MAX
                 IF  SKT2110TMP-KOHNUM(IDX2)  >   SORT-KOHNUM(IDX1 - 1)
                   IF  SKT2110TMP-KOHNUM(IDX2)  <  WRK-KOHNUM
                     MOVE  SKT2110TMP-KOHNUM(IDX2)  TO WRK-KOHNUM
                     MOVE  IDX2          TO    IDX3
                   END-IF
                 END-IF
               END-PERFORM
               MOVE  SKT2110TMP-KOH-TBL(IDX3)  TO  SORT-KOH-TBL(IDX1)
               MOVE  "999"  TO    WRK-KOHNUM
               ADD   1    TO    IDX1
             END-PERFORM
           END-IF
      *
           INITIALIZE             SKT2110TMP-KOH-AREA
           MOVE  SORT-TBL   TO    SKT2110TMP-KOH-AREA
      *
           DISPLAY "一般ソート後１" SKT2110TMP-KOHNUM(1)
           DISPLAY "一般ソート後２" SKT2110TMP-KOHNUM(2)
           DISPLAY "一般ソート後３" SKT2110TMP-KOHNUM(3)
      *
      *    老人公費分ソート
           INITIALIZE  SORT-AREA
      *    公費数を取得
           MOVE    ZERO    TO    IDX-MAX
                                 IDX2 
           MOVE    SPACE   TO    WRK-KOHNUM
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >  20
             IF  SKT2110TMP-RKOHNUM(IDX1)  =  SPACE
               MOVE  20    TO    IDX1
             ELSE
               IF    WRK-KOHNUM               =   SPACE   OR
                     SKT2110TMP-RKOHNUM(IDX1) <   WRK-KOHNUM
                 MOVE     IDX1                TO  IDX2
                 MOVE     SKT2110TMP-RKOHNUM(IDX1) TO  WRK-KOHNUM
               END-IF
               ADD   1     TO    IDX-MAX
             END-IF
           END-PERFORM
           IF     IDX2     >     ZERO
             MOVE    SKT2110TMP-RKOH-TBL(IDX2) TO  SORT-KOH-TBL(1)
           END-IF
      
           DISPLAY "老人ソート前１" SKT2110TMP-RKOHNUM(1)
           DISPLAY "老人ソート前２" SKT2110TMP-RKOHNUM(2)
           DISPLAY "老人ソート前３" SKT2110TMP-RKOHNUM(3)
      *
           IF      IDX-MAX    >     ZERO
             MOVE    2    TO    IDX1
             MOVE    1    TO    IDX2
             MOVE    1    TO    IDX3
             MOVE   "999"   TO    WRK-KOHNUM
             PERFORM    UNTIL    IDX1  >    IDX-MAX
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >    IDX-MAX
                 IF  SKT2110TMP-RKOHNUM(IDX2)  >   SORT-KOHNUM(IDX1 - 1)
                   IF  SKT2110TMP-RKOHNUM(IDX2)  <  WRK-KOHNUM
                     MOVE  SKT2110TMP-RKOHNUM(IDX2)  TO WRK-KOHNUM
                     MOVE  IDX2          TO    IDX3
                   END-IF
                 END-IF
               END-PERFORM
               MOVE  SKT2110TMP-RKOH-TBL(IDX3) TO  SORT-KOH-TBL(IDX1)
               MOVE  "999"  TO    WRK-KOHNUM
               ADD   1    TO    IDX1
             END-PERFORM
           END-IF
      *
           INITIALIZE             SKT2110TMP-RKOH-AREA
           MOVE  SORT-TBL   TO    SKT2110TMP-RKOH-AREA
      *
           DISPLAY "老人ソート後１" SKT2110TMP-RKOHNUM(1)
           DISPLAY "老人ソート後２" SKT2110TMP-RKOHNUM(2)
           DISPLAY "老人ソート後３" SKT2110TMP-RKOHNUM(3)
      *
      *
           .
       KOH-SORT-EXT.
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
      *
      *###（地方公費仕様）
                   MOVE    LNK-PRTKANRI-SRYYM
                                           TO  RECE10-SKYYM
                   MOVE    LNK-PRTKANRI-SKYYMD
                                           TO  RECE10-UPDYMD
      *
                   MOVE    RECE10-REC      TO  MCPDATA-REC
                   MOVE    "DBUPDATE"      TO  MCP-FUNC
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
                       MOVE  "*** SOKATU2110 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                   MOVE  "*** SOKATU2110 SEIKYU INVALID ***"
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
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE    SPA-HOSPNUM             TO  HKNJA-HOSPNUM
           MOVE    SKT2110TMP-HKNJANUM     TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM  925-HKNJAINF-READ-SEC
           ELSE
               DISPLAY  "HKNCOBMI SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNJAINF-REC
             MOVE  ZERO         TO  FLG-HKNJAINF
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
      *   保険番号情報セット
      *****************************************************************
       950-HKNNUM-SET-SEC  SECTION.
      *
           INITIALIZE  HKNNUM-REC
           MOVE    SPA-HOSPNUM                 TO  HKN-HOSPNUM
           MOVE    WRK-KOHNUM                  TO  HKN-HKNNUM
           MOVE    LNK-PRTKANRI-SRYYM(1:6)     TO  HKN-TEKSTYMD(1:6)
           MOVE    "01"                        TO  HKN-TEKSTYMD(7:2)
           MOVE    HKN-TEKSTYMD                TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC                  TO  MCPDATA-REC
           MOVE    "DBSELECT"                  TO  MCP-FUNC
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
           MOVE    "key5"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    "tbl_hknnum"   TO  MCP-TABLE
               MOVE    "key5"         TO  MCP-PATHNAME
               PERFORM  955-HKNNUM-READ-SEC
           ELSE
               DISPLAY  "HKNNUM SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
           MOVE    "key5"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"        USING
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
           ELSE
             INITIALIZE  HKNNUM-REC
             MOVE  1  TO  FLG-HKNNUM
           END-IF
      *
           .
       955-HKNNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *  診療情報セット
      *****************************************************************
       960-SRYACT-SET-SEC  SECTION.
      *
           INITIALIZE                     SRYACT-REC
           MOVE  SPA-HOSPNUM          TO  SRY-HOSPNUM
           MOVE  RECE10-NYUGAIKBN     TO  SRY-NYUGAIKBN
           MOVE  RECE10-PTID          TO  SRY-PTID
           MOVE  RECE10-SRYYM         TO  SRY-SRYYM
           MOVE  SRYACT-REC           TO  MCPDATA-REC
           MOVE  "DBSELECT"           TO  MCP-FUNC
           MOVE  "tbl_sryact"         TO  MCP-TABLE
           MOVE  "key4"               TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"          USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
               PERFORM  965-SRYACT-READ-SEC
      *    在総診チェック
               PERFORM  UNTIL  FLG-SRYACT  =  1
                 PERFORM  965-ZAISOU-CHK-SEC
      *
                 PERFORM  965-SRYACT-READ-SEC
               END-PERFORM
           ELSE
               DISPLAY  "SRYACT SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE  "tbl_sryact"         TO  MCP-TABLE
           MOVE  "key4"               TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"          USING
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
           MOVE  "DBFETCH"        TO  MCP-FUNC
           MOVE  "tbl_sryact"     TO  MCP-TABLE
           MOVE  "key4"           TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"          USING
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
               IF  SRY-SRYCD(IDX4)  =  CONS-ZAITAKU1  OR
                                       CONS-ZAITAKU2  OR
                                       CONS-ZAITAKU3  OR
                                       CONS-ZAITAKU4  OR
                                       CONS-ZAITAKU5  OR
                                       CONS-ZAITAKU6  OR
                                       CONS-ZAITAKU7  OR
                                       CONS-ZAITAKU8  OR
                                       CONS-ZAITAKU9  OR
                                       CONS-ZAITAKU10 OR
                                       CONS-ZAITAKU11 OR
                                       CONS-ZAITAKU12 OR
                                       CONS-ZAITAKU13 OR
                                       CONS-ZAITAKU14 OR
                                       CONS-ZAITAKU15 OR
                                       CONS-ZAITAKU16 OR
                                       CONS-ZAITAKU17 OR
                                       CONS-ZAITAKU18 OR
                                       CONS-ZAITAKU19 OR
                                       CONS-ZAITAKU20 OR
                                       CONS-ZAITAKU21 OR
                                       CONS-ZAITAKU22 OR
                                       CONS-ZAITAKU23 OR
                                       CONS-ZAITAKU24 OR
                                       CONS-ZAITAKU25 OR
                                       CONS-ZAITAKU26 OR
                                       CONS-ZAITAKU27 OR
                                       CONS-ZAITAKU28 OR
                                       CONS-ZAITAKU29 OR
                                       CONS-ZAITAKU30 OR
                                       CONS-ZAITAKU31 OR
                                       CONS-ZAITAKU32 OR
                                       CONS-ZAITAKU33 OR
                                       CONS-ZAITAKU34 OR
                                       CONS-ZAITAKU35 OR
                                       CONS-ZAITAKU36 OR
                                       CONS-ZAITAKU37 OR
                                       CONS-ZAITAKU38
                                   OR  CONS-ZAITAKU39
                                   OR  CONS-ZAITAKU40
                                   OR  CONS-ZAITAKU41
                                   OR  CONS-ZAITAKU42
                                   OR  CONS-ZAITAKU43
                                   OR  CONS-ZAITAKU44
                                   OR  CONS-ZAITAKU45
                                   OR  CONS-ZAITAKU46
                                   OR  CONS-ZAITAKU47
                                   OR  CONS-ZAITAKU48
                                   OR  CONS-ZAITAKU49
                                   OR  CONS-ZAITAKU50
                                   OR  CONS-ZAITAKU51
                                   OR  CONS-ZAITAKU52
                                   OR  CONS-ZAITAKU53
                                   OR  CONS-ZAITAKU54
                                   OR  CONS-ZAITAKU55
                                   OR  CONS-ZAITAKU56
                                   OR  CONS-ZAITAKU57
                                   OR  CONS-ZAITAKU58
                                   OR  CONS-ZAITAKU59
                                   OR  CONS-ZAITAKU60
                                   OR  CONS-ZAITAKU61
                                   OR  CONS-ZAITAKU62
                                   OR  CONS-ZAITAKU63
                                   OR  CONS-ZAITAKU64
                                   OR  CONS-ZAITAKU65
                                   OR  CONS-ZAITAKU66
                                   OR  CONS-ZAITAKU67
                                   OR  CONS-ZAITAKU68
                                   OR  CONS-ZAITAKU69
                                   OR  CONS-ZAITAKU70
                                   OR  CONS-ZAITAKU71
                                   OR  CONS-ZAITAKU72
                                   OR  CONS-ZAITAKU73
                                   OR  CONS-ZAITAKU74
                                   OR  CONS-ZAITAKU75
                                   OR  CONS-ZAITAKU76
                                   OR  CONS-ZAITAKU77
                                   OR  CONS-ZAITAKU78
                                   OR  CONS-ZAITAKU79
                                   OR  CONS-ZAITAKU80
                                   OR  CONS-ZAITAKU81
                                   OR  CONS-ZAITAKU82
                                   OR  CONS-ZAITAKU83
                                   OR  CONS-ZAITAKU84
                                   OR  CONS-ZAITAKU85
                                   OR  CONS-ZAITAKU86
                                   OR  CONS-ZAITAKU87
                                   OR  CONS-ZAITAKU88
                                   OR  CONS-ZAITAKU89
                                   OR  CONS-ZAITAKU90
                                   OR  CONS-ZAITAKU91
                                   OR  CONS-ZAITAKU92
                                   OR  CONS-ZAITAKU93
                                   OR  CONS-ZAITAKU94
                                   OR  CONS-ZAITAKU95
                                   OR  CONS-ZAITAKU96
                                   OR  CONS-ZAITAKU97
                                   OR  CONS-ZAITAKU98
                                   OR  CONS-ZAITAKU99
                                   OR  CONS-ZAITAKU100
                                   OR  CONS-ZAITAKU101
                                   OR  CONS-ZAITAKU102
                                   OR  CONS-ZAITAKU103
                                   OR  CONS-ZAITAKU104
                                   OR  CONS-ZAITAKU105
                                   OR  CONS-ZAITAKU106
                                   OR  CONS-ZAITAKU107
                                   OR  CONS-ZAITAKU108
                                   OR  CONS-ZAITAKU109
                                   OR  CONS-ZAITAKU110
                                   OR  CONS-ZAITAKU111
                                   OR  CONS-ZAITAKU112
                                   OR  CONS-ZAITAKU113
                                   OR  CONS-ZAITAKU114
                                   OR  CONS-ZAITAKU115
                                   OR  CONS-ZAITAKU116
                                   OR  CONS-ZAITAKU117
      *
                 MOVE  "1"  TO  SKT2110TMP-ZAITAKU
               END-IF
             END-IF
           END-PERFORM
      *
           .
       965-ZAISOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
      *
           MOVE    SPA-HOSPNUM          TO    RECE10-HOSPNUM
           MOVE   LNK-PRTKANRI-SRYYM    TO    RECE10-SKYYM.
           IF    WRK-PARA-HKNJANUM  NOT = ZERO
             MOVE  WRK-PARA-HKNJANUM  TO  RECE10-XXSTHKNJANUM
                                          RECE10-XXEDHKNJANUM
           ELSE
               MOVE    SPACE               TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"          TO  RECE10-XXEDHKNJANUM
           END-IF
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  000000                  TO  RECE10-SRYYM
             MOVE  999999                  TO  RECE10-XXSRYYM
           END-IF
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
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
               MOVE    ZERO            TO  FLG-END
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
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
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
       900-SKTTMP-READ-SEC             SECTION.
      *
           READ    SKT2110TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT2110TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT2110TMP
           END-READ
           .
       900-SKTTMP-READ-EXT.
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
           MOVE    "DBSELECT"                   TO  MCP-FUNC
           MOVE    "tbl_syskanri"           TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                    TO  MCP-FUNC
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key10"                 TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"             USING
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
           MOVE    "DBCLOSECURSOR"              TO  MCP-FUNC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key10"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
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
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_seikyu"               TO  MCP-TABLE
           MOVE    "key"                      TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
                                              MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
               MOVE    "tbl_seikyu"           TO  MCP-TABLE
               MOVE    "key"                  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"            USING
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
           MOVE    "tbl_seikyu"               TO  MCP-TABLE
           MOVE    "key"                      TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"                USING
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
           READ  SKT2110TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    全角変換処理 04/12/02 INS
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
      *    システム管理マスタ読込（ＮＥＸＴ）
      *****************************************************************
       900-SYSKANRI-READ-N-SEC         SECTION.
      *
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
           MOVE    "01"                TO  SYS-1903-STYUKYMD (7:2)
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
      *
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
      *
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
