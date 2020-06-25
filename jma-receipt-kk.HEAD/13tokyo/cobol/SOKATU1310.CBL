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
       PROGRAM-ID.             SOKATU1310.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 東京・診療報酬請求書(国保請求書)
      *  管理者            :
      *  作成日付    作業者        記述
      *  05/05/18    有村          再作成(2.5.0対応)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    内田    07/09/12    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *  01.00.02    内田    08/04/22    帳票様式の変更に伴う修正
      *                                  政令指定都市は市でまとめる
      *                                  SPA-AREA初期化
      *  01.00.03    内田    08/08/19    在総診の診療があれば別集計とする
      *  01.00.04    吉川    09/01/26    シス管「2007」の公費まとめ対応
      *  01.00.05    吉川    09/11/18    シス管「2007」のまとめ公費で
      *                                  6つ目以降がまとまらない不具合を修正
      *  01.00.06    吉川    10/08/03    オンライン返戻対応
      *                                  特別療養費国保対応
      *                                  代表保険者番号追加(146001)
      *  01.00.07    吉川    10/10/08    再印刷時の帳票タイトル修正
      *  01.00.08    吉川    10/12/15    open-cobol1.0対応
      *  01.00.09    吉川    11/03/15    東北地方太平洋沖地震対応（対象者別集計）
      *  01.00.10    吉川    12/05/16    H24年4月に新設された在総診対応
      *  01.00.11    吉川    12/12/21    プラグイン対応
      *                                  医療機関情報編集対応
      *  01.00.12    吉川    13/07/26    在宅の対象とする診療コード追加
      *                                  プログラムオプション対応
      *  01.00.13    吉川    14/05/21    前期高齢者２割対応
      *  01.00.14    萩野    14/06/06    在宅の対象とする診療コード追加
      *  01.00.15    吉川    14/10/08    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.16    萩野    14/12/05    返戻時、再印刷画面のタイトル修正
      *  01.00.17    門間    16/03/08    医療機関編集情報への開設者名追加対応
      *  01.00.18    萩野    16/05/12    在宅の対象とする診療コード追加
      *  01.00.19    門間    16/10/04    公費番号まとめ情報の公費番号の追加
      *  01.00.20    門間    18/01/09    医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *                                  SITEKBN変更（動作に変更なし）
      *  01.00.21    門間    18/11/02    修正履歴コメント削除
      *                                  保険番号情報の取得処理を修正
      *  01.00.22    門間    19/09/02    開設者氏名の印字文字数の増加
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.

      *    請求書一時ファイル
           SELECT  SKT1310TMP-FILE ASSIGN    SKT1310TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT1310TMP-KEY
                                   FILE    STATUS  IS  STS-SKT1310.
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
       FD  SKT1310TMP-FILE.
           COPY    "CPSKT1310TMP.INC".
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
                                   BY         //SKT1310TP//.
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
           COPY    "SKT1310.INC".
           COPY    "SKT1320.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT1310         PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-YUSEN           PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT1310TMP      PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
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
           03  IDZ                 PIC 9(04).
      *
      *    パラメタ領域
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
           03  WRK-CNT             PIC 9(02).
           03  WRK-KOHNUM          PIC X(03).
           03  WRK-HKNJANUM        PIC X(06).
           03  WRK-HKNJANAME       PIC X(20).
           03  WRK-ITBFTN-G        PIC 9(09).
           03  WRK-ITBFTN          PIC 9(08).
           03  WRK-KOHNAME         PIC X(10).
      *
           03  WRK-Z2              PIC ZZ.
           03  WRK-Z3              PIC ZZZ.
           03  WRK-Z4              PIC ZZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z6              PIC ZZ,ZZZ.
           03  WRK-Z7              PIC ZZZ,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z10             PIC ZZ,ZZZ,ZZZ.
           03  WRK-Z11             PIC ZZZ,ZZZ,ZZZ.
           03  WRK-Z13             PIC Z,ZZZ,ZZZ,ZZZ.
      *
           03  WRK-RECEERR         PIC X(200).
      *    文字変換
           03  WRK-MAE-INPUT       PIC X(200).
           03  WRK-OUT-INPUT       PIC X(200).
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
      *    公費番号まとめエリア
       01  WRK-2007-AREA.
           03  WRK-2007-TBL          OCCURS  100.
               05  WRK-2007-MEISYO   PIC X(44).
               05  WRK-2007-KOH-AREA.
                   07  WRK-2007-KOHNUM
                                     PIC X(03)   OCCURS  30.
           03  WRK-2007-MAX          PIC 9(03).
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
      *
       01  SORT-AREA.
           03  SORT-TBL.
               05  SORT-KOH-TBL  OCCURS  20.
                   07  SORT-KOHNUM           PIC  X(03).
                   07  SORT-KOHNAME          PIC  X(10).
                   07  SORT-KKENSU           PIC  9(06).
                   07  SORT-KNISSU           PIC  9(06).
                   07  SORT-KTENSU           PIC  9(10).
                   07  SORT-KITBFTN          PIC  9(10).
                   07  SORT-S-KKENSU         PIC  9(06).
                   07  SORT-S-KNISSU         PIC  9(06).
                   07  SORT-S-KKINGK         PIC  9(10).
                   07  SORT-S-KFTNGK         PIC  9(10).
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "COMMON-SPA".
      *
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-200804   PIC X(06)   VALUE   "200804".
      *    在宅総合・在宅末期（在医総）
           03  WRK-CONS-ZAITAKU1       PIC X(09) VALUE "114007610".
           03  WRK-CONS-ZAITAKU2       PIC X(09) VALUE "114007710".
           03  WRK-CONS-ZAITAKU3       PIC X(09) VALUE "114007510".
           03  WRK-CONS-ZAITAKU4       PIC X(09) VALUE "114012210".
           03  WRK-CONS-ZAITAKU5       PIC X(09) VALUE "114012310".
           03  WRK-CONS-ZAITAKU6       PIC X(09) VALUE "114012410".
           03  WRK-CONS-ZAITAKU7       PIC X(09) VALUE "114018710".
           03  WRK-CONS-ZAITAKU8       PIC X(09) VALUE "114018810".
           03  WRK-CONS-ZAITAKU9       PIC X(09) VALUE "114018910".
           03  WRK-CONS-ZAITAKU10      PIC X(09) VALUE "114019010".
           03  WRK-CONS-ZAITAKU11      PIC X(09) VALUE "114019510".
           03  WRK-CONS-ZAITAKU12      PIC X(09) VALUE "114019610".
           03  WRK-CONS-ZAITAKU13      PIC X(09) VALUE "114019710".
           03  WRK-CONS-ZAITAKU14      PIC X(09) VALUE "114019810".
           03  WRK-CONS-ZAITAKU15      PIC X(09) VALUE "114013010".
           03  WRK-CONS-ZAITAKU16      PIC X(09) VALUE "114013110".
           03  WRK-CONS-ZAITAKU17      PIC X(09) VALUE "114013210".
           03  WRK-CONS-ZAITAKU18      PIC X(09) VALUE "114013310".
           03  WRK-CONS-ZAITAKU19      PIC X(09) VALUE "114019110".
           03  WRK-CONS-ZAITAKU20      PIC X(09) VALUE "114019210".
           03  WRK-CONS-ZAITAKU21      PIC X(09) VALUE "114019310".
           03  WRK-CONS-ZAITAKU22      PIC X(09) VALUE "114019410".
           03  WRK-CONS-ZAITAKU23      PIC X(09) VALUE "114023110".
           03  WRK-CONS-ZAITAKU24      PIC X(09) VALUE "114023210".
           03  WRK-CONS-ZAITAKU25      PIC X(09) VALUE "114023310".
           03  WRK-CONS-ZAITAKU26      PIC X(09) VALUE "114023410".
           03  WRK-CONS-ZAITAKU27      PIC X(09) VALUE "114023510".
           03  WRK-CONS-ZAITAKU28      PIC X(09) VALUE "114023610".
           03  WRK-CONS-ZAITAKU29      PIC X(09) VALUE "114023710".
           03  WRK-CONS-ZAITAKU30      PIC X(09) VALUE "114023810".
           03  WRK-CONS-ZAITAKU31      PIC X(09) VALUE "114023910".
           03  WRK-CONS-ZAITAKU32      PIC X(09) VALUE "114024010".
           03  WRK-CONS-ZAITAKU33      PIC X(09) VALUE "114024110".
           03  WRK-CONS-ZAITAKU34      PIC X(09) VALUE "114024210".
           03  WRK-CONS-ZAITAKU35      PIC X(09) VALUE "114024310".
           03  WRK-CONS-ZAITAKU36      PIC X(09) VALUE "114024410".
           03  WRK-CONS-ZAITAKU37      PIC X(09) VALUE "114024510".
           03  WRK-CONS-ZAITAKU38      PIC X(09) VALUE "114024610".
           03  WRK-CONS-ZAITAKU39      PIC X(09) VALUE "114024910".
           03  WRK-CONS-ZAITAKU40      PIC X(09) VALUE "114025010".
           03  WRK-CONS-ZAITAKU41      PIC X(09) VALUE "114025110".
           03  WRK-CONS-ZAITAKU42      PIC X(09) VALUE "114025210".
           03  WRK-CONS-ZAITAKU43      PIC X(09) VALUE "114025310".
           03  WRK-CONS-ZAITAKU44      PIC X(09) VALUE "114025410".
           03  WRK-CONS-ZAITAKU45      PIC X(09) VALUE "114025510".
           03  WRK-CONS-ZAITAKU46      PIC X(09) VALUE "114025610".
           03  WRK-CONS-ZAITAKU47      PIC X(09) VALUE "114025710".
           03  WRK-CONS-ZAITAKU48      PIC X(09) VALUE "114025810".
           03  WRK-CONS-ZAITAKU49      PIC X(09) VALUE "114025910".
           03  WRK-CONS-ZAITAKU50      PIC X(09) VALUE "114026010".
           03  WRK-CONS-ZAITAKU51      PIC X(09) VALUE "114026110".
           03  WRK-CONS-ZAITAKU52      PIC X(09) VALUE "114026210".
           03  WRK-CONS-ZAITAKU53      PIC X(09) VALUE "114026310".
           03  WRK-CONS-ZAITAKU54      PIC X(09) VALUE "114026410".
      *
      *    在総診（２０１６年４月診療分〜）
      *    在がん医総（在支診等）（処方せんあり）
           03  WRK-CONS-ZAITAKU55      PIC X(09) VALUE "114007610".
      *    在がん医総（在支診等）（処方せんなし）
           03  WRK-CONS-ZAITAKU56      PIC X(09) VALUE "114007710".
      *    在医総・在医総管（ＤＵＭＭＹ）
           03  WRK-CONS-ZAITAKU57      PIC X(09) VALUE "099140003".
      *
      *    在がん医総（機能強化した在支診等）（病床あり）（処方せんあり）
           03  WRK-CONS-ZAITAKU58      PIC X(09) VALUE "114019510".
      *    在がん医総（機能強化した在支診等）（病床あり）（処方せんなし）
           03  WRK-CONS-ZAITAKU59      PIC X(09) VALUE "114019610".
      *    在がん医総（機能強化した在支診等）（病床なし）（処方せんあり）
           03  WRK-CONS-ZAITAKU60      PIC X(09) VALUE "114019710".
      *    在がん医総（機能強化した在支診等）（病床なし）（処方せんなし）
           03  WRK-CONS-ZAITAKU61      PIC X(09) VALUE "114019810".
      *
      *    在医総管（機能強化在支診等・病床有・難病等月２回以上・１人）
           03  WRK-CONS-ZAITAKU62      PIC X(09) VALUE "114030710".
      *    在医総管（機能強化在支診等・病床有・難病等月２回以上・２〜９人）
           03  WRK-CONS-ZAITAKU63      PIC X(09) VALUE "114030810".
      *    在医総管（機能強化在支診等・病床有・難病等月２回以上・１０人〜）
           03  WRK-CONS-ZAITAKU64      PIC X(09) VALUE "114030910".
      *    在医総管（機能強化在支診等・病床有・月２回以上・１人）
           03  WRK-CONS-ZAITAKU65      PIC X(09) VALUE "114031010".
      *    在医総管（機能強化在支診等・病床有・月２回以上・２〜９人）
           03  WRK-CONS-ZAITAKU66      PIC X(09) VALUE "114031110".
      *    在医総管（機能強化在支診等・病床有・月２回以上・１０人〜）
           03  WRK-CONS-ZAITAKU67      PIC X(09) VALUE "114031210".
      *    在医総管（機能強化在支診等・病床有・月１回・１人）
           03  WRK-CONS-ZAITAKU68      PIC X(09) VALUE "114031310".
      *    在医総管（機能強化在支診等・病床有・月１回・２〜９人）
           03  WRK-CONS-ZAITAKU69      PIC X(09) VALUE "114031410".
      *    在医総管（機能強化在支診等・病床有・月１回・１０人〜）
           03  WRK-CONS-ZAITAKU70      PIC X(09) VALUE "114031510".
      *    在医総管（機能強化在支診等・病床無・難病等月２回以上・１人）
           03  WRK-CONS-ZAITAKU71      PIC X(09) VALUE "114031610".
      *    在医総管（機能強化在支診等・病床無・難病等月２回以上・２〜９人）
           03  WRK-CONS-ZAITAKU72      PIC X(09) VALUE "114031710".
      *    在医総管（機能強化在支診等・病床無・難病等月２回以上・１０人〜）
           03  WRK-CONS-ZAITAKU73      PIC X(09) VALUE "114031810".
      *    在医総管（機能強化在支診等・病床無・月２回以上・１人）
           03  WRK-CONS-ZAITAKU74      PIC X(09) VALUE "114031910".
      *    在医総管（機能強化在支診等・病床無・月２回以上・２〜９人）
           03  WRK-CONS-ZAITAKU75      PIC X(09) VALUE "114032010".
      *    在医総管（機能強化在支診等・病床無・月２回以上・１０人〜）
           03  WRK-CONS-ZAITAKU76      PIC X(09) VALUE "114032110".
      *    在医総管（機能強化在支診等・病床無・月１回・１人）
           03  WRK-CONS-ZAITAKU77      PIC X(09) VALUE "114032210".
      *    在医総管（機能強化在支診等・病床無・月１回・２〜９人）
           03  WRK-CONS-ZAITAKU78      PIC X(09) VALUE "114032310".
      *    在医総管（機能強化在支診等・病床無・月１回・１０人〜）
           03  WRK-CONS-ZAITAKU79      PIC X(09) VALUE "114032410".
      *    在医総管（在支診等・難病等月２回以上・１人）
           03  WRK-CONS-ZAITAKU80      PIC X(09) VALUE "114032510".
      *    在医総管（在支診等・難病等月２回以上・２〜９人）
           03  WRK-CONS-ZAITAKU81      PIC X(09) VALUE "114032610".
      *    在医総管（在支診等・難病等月２回以上・１０人〜）
           03  WRK-CONS-ZAITAKU82      PIC X(09) VALUE "114032710".
      *    在医総管（在支診等・月２回以上・１人）
           03  WRK-CONS-ZAITAKU83      PIC X(09) VALUE "114032810".
      *    在医総管（在支診等・月２回以上・２〜９人）
           03  WRK-CONS-ZAITAKU84      PIC X(09) VALUE "114032910".
      *    在医総管（在支診等・月２回以上・１０人〜）
           03  WRK-CONS-ZAITAKU85      PIC X(09) VALUE "114033010".
      *    在医総管（在支診等・月１回・１人）
           03  WRK-CONS-ZAITAKU86      PIC X(09) VALUE "114033110".
      *    在医総管（在支診等・月１回・２〜９人）
           03  WRK-CONS-ZAITAKU87      PIC X(09) VALUE "114033210".
      *    在医総管（在支診等・月１回・１０人〜）
           03  WRK-CONS-ZAITAKU88      PIC X(09) VALUE "114033310".
      *    在医総管（在支診等以外・難病等月２回以上・１人）
           03  WRK-CONS-ZAITAKU89      PIC X(09) VALUE "114033410".
      *    在医総管（在支診等以外・難病等月２回以上・２〜９人）
           03  WRK-CONS-ZAITAKU90      PIC X(09) VALUE "114033510".
      *    在医総管（在支診等以外・難病等月２回以上・１０人〜）
           03  WRK-CONS-ZAITAKU91      PIC X(09) VALUE "114033610".
      *    在医総管（在支診等以外・月２回以上・１人）
           03  WRK-CONS-ZAITAKU92      PIC X(09) VALUE "114033710".
      *    在医総管（在支診等以外・月２回以上・２〜９人）
           03  WRK-CONS-ZAITAKU93      PIC X(09) VALUE "114033810".
      *    在医総管（在支診等以外・月２回以上・１０人〜）
           03  WRK-CONS-ZAITAKU94      PIC X(09) VALUE "114033910".
      *    在医総管（在支診等以外・月１回・１人）
           03  WRK-CONS-ZAITAKU95      PIC X(09) VALUE "114034010".
      *    在医総管（在支診等以外・月１回・２〜９人）
           03  WRK-CONS-ZAITAKU96      PIC X(09) VALUE "114034110".
      *    在医総管（在支診等以外・月１回・１０人〜）
           03  WRK-CONS-ZAITAKU97      PIC X(09) VALUE "114034210".
      *
      *    施医総管（機能強化在支診等・病床有・難病等月２回以上・１人）
           03  WRK-CONS-ZAITAKU98      PIC X(09) VALUE "114035510".
      *    施医総管（機能強化在支診等・病床有・難病等月２回以上・２〜９人）
           03  WRK-CONS-ZAITAKU99      PIC X(09) VALUE "114035610".
      *    施医総管（機能強化在支診等・病床有・難病等月２回以上・１０人〜）
           03  WRK-CONS-ZAITAKU100      PIC X(09) VALUE "114035710".
      *    施医総管（機能強化在支診等・病床有・月２回以上・１人）
           03  WRK-CONS-ZAITAKU101      PIC X(09) VALUE "114035810".
      *    施医総管（機能強化在支診等・病床有・月２回以上・２〜９人）
           03  WRK-CONS-ZAITAKU102      PIC X(09) VALUE "114035910".
      *    施医総管（機能強化在支診等・病床有・月２回以上・１０人〜）
           03  WRK-CONS-ZAITAKU103      PIC X(09) VALUE "114036010".
      *    施医総管（機能強化在支診等・病床有・月１回・１人）
           03  WRK-CONS-ZAITAKU104      PIC X(09) VALUE "114036110".
      *    施医総管（機能強化在支診等・病床有・月１回・２〜９人）
           03  WRK-CONS-ZAITAKU105      PIC X(09) VALUE "114036210".
      *    施医総管（機能強化在支診等・病床有・月１回・１０人〜）
           03  WRK-CONS-ZAITAKU106      PIC X(09) VALUE "114036310".
      *    施医総管（機能強化在支診等・病床無・難病等月２回以上・１人）
           03  WRK-CONS-ZAITAKU107      PIC X(09) VALUE "114036410".
      *    施医総管（機能強化在支診等・病床無・難病等月２回以上・２〜９人）
           03  WRK-CONS-ZAITAKU108      PIC X(09) VALUE "114036510".
      *    施医総管（機能強化在支診等・病床無・難病等月２回以上・１０人〜）
           03  WRK-CONS-ZAITAKU109      PIC X(09) VALUE "114036610".
      *    施医総管（機能強化在支診等・病床無・月２回以上・１人）
           03  WRK-CONS-ZAITAKU110      PIC X(09) VALUE "114036710".
      *    施医総管（機能強化在支診等・病床無・月２回以上・２〜９人）
           03  WRK-CONS-ZAITAKU111      PIC X(09) VALUE "114036810".
      *    施医総管（機能強化在支診等・病床無・月２回以上・１０人〜）
           03  WRK-CONS-ZAITAKU112      PIC X(09) VALUE "114036910".
      *    施医総管（機能強化在支診等・病床無・月１回・１人）
           03  WRK-CONS-ZAITAKU113      PIC X(09) VALUE "114037010".
      *    施医総管（機能強化在支診等・病床無・月１回・２〜９人）
           03  WRK-CONS-ZAITAKU114      PIC X(09) VALUE "114037110".
      *    施医総管（機能強化在支診等・病床無・月１回・１０人〜）
           03  WRK-CONS-ZAITAKU115      PIC X(09) VALUE "114037210".
      *    施医総管（在支診等・難病等月２回以上・１人）
           03  WRK-CONS-ZAITAKU116      PIC X(09) VALUE "114037310".
      *    施医総管（在支診等・難病等月２回以上・２〜９人）
           03  WRK-CONS-ZAITAKU117      PIC X(09) VALUE "114037410".
      *    施医総管（在支診等・難病等月２回以上・１０人〜）
           03  WRK-CONS-ZAITAKU118      PIC X(09) VALUE "114037510".
      *    施医総管（在支診等・月２回以上・１人）
           03  WRK-CONS-ZAITAKU119      PIC X(09) VALUE "114037610".
      *    施医総管（在支診等・月２回以上・２〜９人）
           03  WRK-CONS-ZAITAKU120      PIC X(09) VALUE "114037710".
      *    施医総管（在支診等・月２回以上・１０人〜）
           03  WRK-CONS-ZAITAKU121      PIC X(09) VALUE "114037810".
      *    施医総管（在支診等・月１回・１人）
           03  WRK-CONS-ZAITAKU122      PIC X(09) VALUE "114037910".
      *    施医総管（在支診等・月１回・２〜９人）
           03  WRK-CONS-ZAITAKU123      PIC X(09) VALUE "114038010".
      *    施医総管（在支診等・月１回・１０人〜）
           03  WRK-CONS-ZAITAKU124      PIC X(09) VALUE "114038110".
      *    施医総管（在支診等以外・難病等月２回以上・１人）
           03  WRK-CONS-ZAITAKU125      PIC X(09) VALUE "114038210".
      *    施医総管（在支診等以外・難病等月２回以上・２〜９人）
           03  WRK-CONS-ZAITAKU126      PIC X(09) VALUE "114038310".
      *    施医総管（在支診等以外・難病等月２回以上・１０人〜）
           03  WRK-CONS-ZAITAKU127      PIC X(09) VALUE "114038410".
      *    施医総管（在支診等以外・月２回以上・１人）
           03  WRK-CONS-ZAITAKU128      PIC X(09) VALUE "114038510".
      *    施医総管（在支診等以外・月２回以上・２〜９人）
           03  WRK-CONS-ZAITAKU129      PIC X(09) VALUE "114038610".
      *    施医総管（在支診等以外・月２回以上・１０人〜）
           03  WRK-CONS-ZAITAKU130      PIC X(09) VALUE "114038710".
      *    施医総管（在支診等以外・月１回・１人）
           03  WRK-CONS-ZAITAKU131      PIC X(09) VALUE "114038810".
      *    施医総管（在支診等以外・月１回・２〜９人）
           03  WRK-CONS-ZAITAKU132      PIC X(09) VALUE "114038910".
      *    施医総管（在支診等以外・月１回・１０人〜）
           03  WRK-CONS-ZAITAKU133      PIC X(09) VALUE "114039010".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者
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
           INITIALIZE                   SPA-AREA
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
           MOVE    WRK-PARA-HOSPNUM   TO  SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SOKATU1310"    TO  JOB-PGID
           MOVE    "国保請求書"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "SKT1310"           TO  SKT1310TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT1310TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT1310TP-HOSPNUM
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  SKT1310TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT1310TP-FULLNAME
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
               MOVE    ZERO                TO  IDZ
               PERFORM VARYING IDX2 FROM    1   BY  1
                       UNTIL   IDX2 >       10
                       OR      SYS-2007-KOHNUM (IDX2)   =   SPACE
                    ADD     1               TO  IDZ
                    MOVE    SYS-2007-KOHNUM (IDX2)
                                           TO  WRK-2007-KOHNUM(IDX1 IDZ)
               END-PERFORM
               PERFORM VARYING IDX2 FROM    1   BY  1
                       UNTIL   IDX2 >       20
                       OR      SYS-2007-KOHNUM1(IDX2)   =   SPACE
                   ADD     1               TO  IDZ
                   MOVE    SYS-2007-KOHNUM1(IDX2)
                                           TO  WRK-2007-KOHNUM(IDX1 IDZ)
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
           MOVE    "SOKATU1310"              TO  WRK-PRGID
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
      *
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
           OPEN     OUTPUT  SKT1310TMP-FILE
           CLOSE            SKT1310TMP-FILE
           OPEN     I-O     SKT1310TMP-FILE
      *
           MOVE     ZERO    TO      FLG-END
           MOVE     ZERO    TO      FLG-UPD
      *
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   UPD-FILE
           CLOSE   SKT1310TMP-FILE
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
           DISPLAY "*** SOKATU1310 IN  "  CNT-RECE10
           DISPLAY "*** SOKATU1310 END ***"
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
      *
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
      *TMP-FILE キーセット
      *
           INITIALIZE       SKT1310TMP-REC
           INITIALIZE       SKT1310TMP-HKNJANUM
      *
           IF  RECE10-HKNJANUM(7:2)  =  SPACE
             MOVE  RECE10-HKNJANUM(1:6)  TO  WRK-HKNJANUM
           ELSE
             MOVE  RECE10-HKNJANUM(3:6)  TO  WRK-HKNJANUM
           END-IF
      *
      *    区の場合代表番号セット
           PERFORM  2002-HKNJANUM-SET-SEC
           MOVE  WRK-HKNJANUM            TO  SKT1310TMP-HKNJANUM
      *
      *    印刷順設定
           IF    WRK-YUSEN        =  SKT1310TMP-HKNJANUM
             MOVE          1   TO    SKT1310TMP-SYUTURYOKU
           ELSE
             MOVE          2   TO    SKT1310TMP-SYUTURYOKU
           END-IF
      *
      *    入外区分設定
           MOVE    RECE10-NYUGAIKBN      TO  SKT1310TMP-NYUGAIKBN
      *
      *    在宅・在医総
           MOVE    RECE10-ZAITAKU        TO  SKT1310TMP-ZAITAKU
      *
      *    在総診・在医総か
           IF  RECE10-ZAITAKU  =  SPACE
             PERFORM  960-SRYACT-SET-SEC
           END-IF
      *
      *    災害対象者は別集計
           IF  RECE10-ETC-YKZFTN (1)  NOT =  ZERO
             MOVE  RECE10-ETC-YKZFTN (1)  TO  SKT1310TMP-GENMEN
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
      *    代表番号セット
      *****************************************************************
       2002-HKNJANUM-SET-SEC                SECTION.
      *
           EVALUATE  WRK-HKNJANUM (1:3)
      *        札幌市の場合、代表番号セット
               WHEN  "014"
                   MOVE  "010017"  TO  WRK-HKNJANUM
      *        仙台市の場合、代表番号セット
               WHEN  "044"
                   MOVE  "040014"  TO  WRK-HKNJANUM
      *        さいたま市の場合、代表番号セット
               WHEN  "114"
                   MOVE  "114009"  TO  WRK-HKNJANUM
      *        千葉市の場合、代表番号セット
               WHEN  "124"
                   MOVE  "124008"  TO  WRK-HKNJANUM
      *        横浜市の場合、代表番号セット
               WHEN  "144"
                   MOVE  "144006"  TO  WRK-HKNJANUM
      *        川崎市の場合、代表番号セット
               WHEN  "145"
                   MOVE  "145003"  TO  WRK-HKNJANUM
      *        相模原市の場合、代表番号セット
               WHEN  "146"
                   MOVE  "146001"  TO  WRK-HKNJANUM
      *        新潟市の場合、代表番号セット
               WHEN  "154"
                   MOVE  "154005"  TO  WRK-HKNJANUM
      *        静岡市の場合、代表番号セット
               WHEN  "224"
                   MOVE  "224006"  TO  WRK-HKNJANUM
      *        浜松市の場合、代表番号セット
               WHEN  "225"
                   MOVE  "225003"  TO  WRK-HKNJANUM
      *        名古屋市の場合、代表番号セット
               WHEN  "234"
                   MOVE  "234005"  TO  WRK-HKNJANUM
      *        京都市の場合、代表番号セット
               WHEN  "264"
                   MOVE  "264002"  TO  WRK-HKNJANUM
      *        大阪市の場合、代表番号セット
               WHEN  "274"
                   MOVE  "274001"  TO  WRK-HKNJANUM
      *        堺市の場合、代表番号セット
               WHEN  "275"
                   MOVE  "275008"  TO  WRK-HKNJANUM
      *        神戸市の場合、代表番号セット
               WHEN  "284"
                   MOVE  "284000"  TO  WRK-HKNJANUM
      *        広島市の場合、代表番号セット
               WHEN  "344"
                   MOVE  "344002"  TO  WRK-HKNJANUM
      *        北九州市の場合、代表番号セット
               WHEN  "404"
                   MOVE  "404004"  TO  WRK-HKNJANUM
      *        福岡市の場合、代表番号セット
               WHEN  "405"
                   MOVE  "405001"  TO  WRK-HKNJANUM
           END-EVALUATE
      *
           .
       2002-HKNJANUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・老人保健分集計処理
      *****************************************************************
       210-HKN-SYUKEI-SEC          SECTION.
      *
      *    老人保健か
           IF  RECE10-RJNHKNNUM  NOT  =  SPACE
             IF  RECE10-KYURATE  =  9
               MOVE  11   TO   IDX1
             ELSE
               MOVE  12   TO   IDX1
             END-IF
           ELSE
      *    一般国保か
             IF  RECE10-HKNNUM  =  "060" OR "068"
               IF  RECE10-AGE  >= 70
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
                     MOVE  5     TO  IDX1
                 ELSE
                   IF  RECE10-AGE  >= 3
      *
                     IF    RECE10-KYURATE  =  7
                       MOVE    4     TO    IDX1
                     ELSE
                       MOVE    3     TO    IDX1
                       MOVE    RECE10-KYURATE  TO  SKT1310TMP-KYURATE
                     END-IF
                   ELSE
                     MOVE    5     TO    IDX1
                   END-IF
                 END-IF
               END-IF
      *
             ELSE
               IF  RECE10-AGE  >=  70
                 EVALUATE  RECE10-KYURATE
                   WHEN  9
                     MOVE    6     TO    IDX1
                   WHEN  8
                     IF  RECE10-RECESYUBETU(4:1)  =  "7" OR "8"
                         MOVE    6    TO    IDX1
                     ELSE
                         MOVE    7    TO    IDX1
                     END-IF
                   WHEN  7
                     MOVE    7     TO    IDX1
                 END-EVALUATE
               ELSE
                 IF  RECE10-HONKZKKBN  =  1
                   MOVE  8    TO   IDX1
                 ELSE
                   IF  (RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804)
                   AND  (RECE10-FLG (1)   =  1)
                       MOVE  10     TO  IDX1
                   ELSE
                     IF  RECE10-AGE  <  3
                       MOVE  10  TO  IDX1
                     ELSE
                       MOVE  9  TO  IDX1
                     END-IF
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF
      *
           PERFORM  2101-SYUKEI-SEC
      *
      **    公費負担分
           IF  RECE10-KOHNUM (1)   NOT =   SPACE  OR
               RECE10-KOHNUM (2)   NOT =   SPACE  OR
               RECE10-KOHNUM (3)   NOT =   SPACE  OR
               RECE10-KOHNUM (4)   NOT =   SPACE
               PERFORM 220-KOH-SYUKEI-SEC
           END-IF
      *
           IF  FLG-SKT1310TMP  =  1
               WRITE    SKT1310TMP-REC
           ELSE
               REWRITE  SKT1310TMP-REC
           END-IF
      *
           MOVE    1    TO  FLG-UPD
      *
           .
       210-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・一般国保分集計処理
      *****************************************************************
       2101-SYUKEI-SEC         SECTION.
      *
      *    療養の給付：件数
           ADD   1                  TO  SKT1310TMP-KENSU (IDX1)
      *    療養の給付:診療実日数
           ADD  RECE10-JNISSU(1)    TO  SKT1310TMP-NISSU(IDX1)
      *    療養の給付：点数
           ADD  RECE10-TOTALTEN(1)  TO  SKT1310TMP-TENSU(IDX1)
      *    療養の給付：一部負担金
           ADD  RECE10-FTNMONEY(1)  TO  SKT1310TMP-ITBFTN(IDX1)
      *
           IF  RECE10-SHOKUJINISSU(1)  >  0
      *    食事療養:件数
             ADD   1                     TO  SKT1310TMP-S-KENSU(IDX1)
           END-IF
      *    食事療養:日数
           ADD  RECE10-SHOKUJINISSU(1)   TO  SKT1310TMP-S-NISSU(IDX1)
      *    食事療養：金額
           ADD  RECE10-SHOKUJIRYOYOHI(1) TO  SKT1310TMP-S-KINGK(IDX1)
      *    食事療養：標準負担額
           ADD  RECE10-SHOKUJIFTN(1)     TO  SKT1310TMP-S-FTNGK(IDX1)
      *
           MOVE    1               TO  FLG-UPD
           .
       2101-SYUKEI-EXT.
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
      *    ０５１と０９１は同じ場所に集計する
             IF    RECE10-KOHNUM(IDX3)  =  "091"
               MOVE    "051"    TO    RECE10-KOHNUM(IDX3)
             END-IF
      *      公費番号まとめ情報があるとき
             IF  WRK-2007-MAX  >  ZERO
               MOVE  SPACE     TO  WRK-KOHNAME
               PERFORM VARYING IDZ1    FROM    1   BY  1
                       UNTIL   IDZ1    >       WRK-2007-MAX
                 PERFORM VARYING IDZ2  FROM    1   BY  1
                        UNTIL   IDZ2  >       20
                         OR WRK-2007-KOHNUM(IDZ1 IDZ2)  =  SPACE
                   IF  RECE10-KOHNUM(IDX3)
                                 =   WRK-2007-KOHNUM(IDZ1 IDZ2)
                     MOVE  WRK-2007-KOHNUM(IDZ1 1)
                                 TO  RECE10-KOHNUM (IDX3)
                     MOVE  WRK-2007-MEISYO (IDZ1)
                                 TO  WRK-KOHNAME
                    MOVE  20            TO  IDZ2
                     MOVE  WRK-2007-MAX  TO  IDZ1
                   END-IF
                 END-PERFORM
               END-PERFORM
             END-IF
      *    公費判別
             PERFORM  VARYING  IDX4  FROM  1  BY  1
                      UNTIL    IDX4  >     20
                      OR       SKT1310TMP-KOHNUM (IDX4) = SPACE
               IF  RECE10-KOHNUM (IDX3)  =  SKT1310TMP-KOHNUM (IDX4)
                   MOVE  IDX4  TO  CNT-KOH
                   MOVE  20    TO  IDX4
               END-IF
             END-PERFORM
      *
      *    該当公費がなかった場合
             IF  IDX4  < 20
               MOVE  IDX4  TO  CNT-KOH
             END-IF
      *
      *    公費名
             MOVE  RECE10-KOHNUM(IDX3)  TO  SKT1310TMP-KOHNUM(CNT-KOH)
      *    公費名称
             MOVE  WRK-KOHNAME          TO  SKT1310TMP-KOHNAME(CNT-KOH)
      *    療養の給付：件数
             ADD   1  TO  SKT1310TMP-KKENSU (CNT-KOH)
      *    療養の給付:診療実日数
             ADD  RECE10-JNISSU(IDX3 + 1)  TO
                               SKT1310TMP-KNISSU(CNT-KOH)
      *    療養の給付：点数
             ADD  RECE10-TOTALTEN(IDX3 + 1)  TO
                               SKT1310TMP-KTENSU(CNT-KOH)
      *
      *    療養の給付：一部負担金
              ADD  RECE10-FTNMONEY(IDX3 + 1)  TO
                              SKT1310TMP-KITBFTN(CNT-KOH)

             IF  RECE10-SHOKUJINISSU(IDX3 + 1)  >  ZERO
      *    食事療養:件数
               ADD   1  TO  SKT1310TMP-S-KKENSU(CNT-KOH)
             END-IF
      *    食事療養:日数
             ADD  RECE10-SHOKUJINISSU(IDX3 + 1)  TO
                               SKT1310TMP-S-KNISSU(CNT-KOH)
      *    食事療養：金額
             ADD  RECE10-SHOKUJIRYOYOHI(IDX3 + 1)  TO
                               SKT1310TMP-S-KKINGK(CNT-KOH)
      *    食事療養：標準負担額
             ADD  RECE10-SHOKUJIFTN(IDX3 + 1)  TO
                               SKT1310TMP-S-KFTNGK(CNT-KOH)
           END-PERFORM
      *
           .
       220-KOH-SYUKEI-EXT.
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
           OPEN     INPUT       SKT1310TMP-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
           MOVE     1           TO       WRK-CNT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =   1
      *
      *      公費印字順編集処理
             PERFORM  KOH-SORT-SEC
      *
             PERFORM  UNTIL  SKT1310TMP-KOHNUM (WRK-CNT) = SPACE
                      AND    FLG-PASS  =  1
               IF    SKT1310TMP-NYUGAIKBN  =  "2"
      *    帳票編集<見出し>処理(外来用)
                 PERFORM 310-HEAD-HEN-SEC
      *    帳票編集<明細>処理(外来用)
                 PERFORM 320-BODY-HEN-SEC
      *    帳票印刷処理(外来用)
                 PERFORM 330-PRINT-OUT-SEC
               ELSE
      *    帳票編集<見出し>処理(入院用)
                 PERFORM 311-HEAD-HEN-SEC
      *    帳票編集<明細>処理(入院用)
                 PERFORM 321-BODY-HEN-SEC
      *    帳票印刷処理(入院用)
                 PERFORM 331-PRINT-OUT-SEC
               END-IF
               MOVE  1  TO  FLG-PASS
             END-PERFORM
             MOVE  ZERO  TO  FLG-PASS
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
             MOVE  1  TO  WRK-CNT
           END-PERFORM
      *
           CLOSE    SKT1310TMP-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理(外来用)
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT1310
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT1310-SRYYM
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD       TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT         TO  SKT1310-HOSPCD
      *    保険者番号
           MOVE    SKT1310TMP-HKNJANUM   TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT         TO  SKT1310-HKNJANUM
      *    請求年月日
           MOVE    WRK-SEIYMDWH          TO  SKT1310-SEIYMD
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT1310-ADRS
           INSPECT SKT1310-ADRS          REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT1310-HOSPNAME
           INSPECT SKT1310-HOSPNAME      REPLACING ALL "  " BY "　"
      *    電話番号
           MOVE    WRK-HOSPTEL           TO  SKT1310-TEL
      *    開設者名
           IF  WRK-KAISETUNAME(41:)  =  SPACE
               MOVE    WRK-KAISETUNAME     TO  SKT1310-KAISETUNAME
           ELSE
               MOVE    WRK-KAISETUNAME     TO  SKT1310-KAISETUNAME2
           END-IF
      *
      *    給付割合
           MOVE    SKT1310TMP-KYURATE    TO  WRK-Z2
           MOVE    WRK-Z2                TO  SKT1310-KYURATE
      *    在総診・在医総
           IF    SKT1310TMP-ZAITAKU  =  "1"
             MOVE    "在"                TO  SKT1310-ZAI1
             MOVE    "◯"                TO  SKT1310-ZAI2
           END-IF
      *
      *    ページ番号
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT1310-NO
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT1310-TOKUBETU
           END-IF
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理(外来用)
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
      *    公費の為の２枚目以上か
           IF  WRK-CNT  =  1
      *    編集処理
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     12
      *    療養の給付：件数
               MOVE  SKT1310TMP-KENSU (IDX1)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT1310-KENSU(IDX1)
      *    療養の給付：日数
               MOVE  SKT1310TMP-NISSU (IDX1)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT1310-NISSU(IDX1)
      *    療養の給付：点数
               MOVE  SKT1310TMP-TENSU (IDX1)   TO  WRK-Z11
               MOVE  WRK-Z11  TO  SKT1310-TENSU(IDX1)
      *    療養の給付：一部負担金
               MOVE  SKT1310TMP-ITBFTN (IDX1)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT1310-ITBFTN(IDX1)
             END-PERFORM
           END-IF
      *
      *    公費の印字
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL   IDX3   >     2
                    OR  WRK-CNT    >    20
                    OR  SKT1310TMP-KOHNUM (WRK-CNT) = SPACE
      *    療養の給付：件数
             MOVE  SKT1310TMP-KKENSU (WRK-CNT)  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT1310-KKENSU (IDX3)
      *    療養の給付：日数
             MOVE  SKT1310TMP-KNISSU (WRK-CNT)  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT1310-KNISSU (IDX3)
      *    療養の給付：点数
             MOVE  SKT1310TMP-KTENSU (WRK-CNT)  TO  WRK-Z11
             MOVE  WRK-Z11  TO  SKT1310-KTENSU (IDX3)
      *    療養の給付：一部負担金
             MOVE  SKT1310TMP-KITBFTN (WRK-CNT)  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT1310-KITBFTN (IDX3)
      *    公費名
             MOVE  SKT1310TMP-KOHNUM(WRK-CNT)  TO  WRK-KOHNUM
             PERFORM  950-HKNNUM-SET-SEC
             MOVE    HKN-HBTNUM           TO  WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT   TO  SKT1310-K-KOHNAME(IDX3)(1:4)
             IF  SKT1310TMP-KOHNAME(WRK-CNT)  NOT =  SPACE
               MOVE  SKT1310TMP-KOHNAME(WRK-CNT)
                                       TO  SKT1310-K-KOHNAME(IDX3)(5:)
             ELSE
               MOVE  HKN-TANSEIDONAME  TO  SKT1310-K-KOHNAME(IDX3)(5:)
             END-IF
      *
             ADD  1  TO  WRK-CNT
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理(外来用)
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
           MOVE   "SKT1310.red"        TO  SPRT-PRTID
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
           MOVE    SKT1310             TO  SPRT-PRTDATA
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
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理(入院用)
      *****************************************************************
       311-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                    SKT1320
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT1320-SRYYM
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD       TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT         TO  SKT1320-HOSPCD
      *    保険者番号
           MOVE    SKT1310TMP-HKNJANUM   TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT         TO  SKT1320-HKNJANUM
      *    請求年月日
           MOVE    WRK-SEIYMDWH          TO  SKT1320-SEIYMD
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT1320-ADRS
           INSPECT SKT1320-ADRS          REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT1320-HOSPNAME
           INSPECT SKT1320-HOSPNAME      REPLACING ALL "  " BY "　"
      *    電話番号
           MOVE    WRK-HOSPTEL           TO  SKT1320-TEL
      *    開設者名
           IF  WRK-KAISETUNAME(41:)  =  SPACE
               MOVE    WRK-KAISETUNAME     TO  SKT1320-KAISETUNAME
           ELSE
               MOVE    WRK-KAISETUNAME     TO  SKT1320-KAISETUNAME2
           END-IF
      *
      *    給付割合
           MOVE    SKT1310TMP-KYURATE    TO  WRK-Z2
           MOVE    WRK-Z2                TO  SKT1320-KYURATE
      *
      *    ページ番号
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT1320-NO
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT1320-TOKUBETU
           END-IF
      *
           .
       311-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理(入院用)
      *****************************************************************
       321-BODY-HEN-SEC      SECTION.
      *
      *    公費の為の２枚目以上か
           IF  WRK-CNT  =  1
      *    編集処理
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     12
      *    療養の給付：件数
               MOVE  SKT1310TMP-KENSU (IDX1)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT1320-KENSU(IDX1)
      *    療養の給付：日数
               MOVE  SKT1310TMP-NISSU (IDX1)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT1320-NISSU(IDX1)
      *    療養の給付：点数
               MOVE  SKT1310TMP-TENSU (IDX1)   TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT1320-TENSU(IDX1)
      *    療養の給付：一部負担金
               MOVE  SKT1310TMP-ITBFTN (IDX1)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT1320-ITBFTN(IDX1)
      *    食事療養・件数
               MOVE  SKT1310TMP-S-KENSU(IDX1)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT1320-S-KENSU(IDX1)
      *    食事療養・日数
               MOVE  SKT1310TMP-S-NISSU(IDX1)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT1320-S-NISSU(IDX1)
      *    食事療養・金額
               MOVE  SKT1310TMP-S-KINGK(IDX1)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT1320-S-KINGK(IDX1)
      *    食事療養・標準負担額
               MOVE  SKT1310TMP-S-FTNGK(IDX1)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT1320-S-FTNGK(IDX1)
             END-PERFORM
           END-IF
      *
      *    公費の印字
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL   IDX3   >     2
                    OR  WRK-CNT    >    20
                    OR  SKT1310TMP-KOHNUM (WRK-CNT) = SPACE
      *    療養の給付：件数
             MOVE  SKT1310TMP-KKENSU (WRK-CNT)  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT1320-KKENSU (IDX3)
      *    療養の給付：日数
             MOVE  SKT1310TMP-KNISSU (WRK-CNT)  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT1320-KNISSU (IDX3)
      *    療養の給付：点数
             MOVE  SKT1310TMP-KTENSU (WRK-CNT)  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT1320-KTENSU (IDX3)
      *    療養の給付：一部負担金
             MOVE  SKT1310TMP-KITBFTN (WRK-CNT)  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT1320-KITBFTN (IDX3)
      *    食事療養・件数
             MOVE  SKT1310TMP-S-KKENSU(WRK-CNT)  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT1320-S-KKENSU(IDX3)
      *    食事療養・日数
             MOVE  SKT1310TMP-S-KNISSU(WRK-CNT)  TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT1320-S-KNISSU(IDX3)
      *    食事療養・金額
             MOVE  SKT1310TMP-S-KKINGK(WRK-CNT)  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT1320-S-KKINGK(IDX3)
      *    食事療養・標準負担額
             MOVE  SKT1310TMP-S-KFTNGK(WRK-CNT)  TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT1320-S-KFTNGK(IDX3)
      *    公費名
             MOVE  SKT1310TMP-KOHNUM(WRK-CNT)  TO  WRK-KOHNUM
             PERFORM  950-HKNNUM-SET-SEC
             MOVE    HKN-HBTNUM           TO  WRK-MAE-INPUT
             PERFORM 2009-HENKAN-SEC
             MOVE    WRK-OUT-INPUT   TO  SKT1320-K-KOHNAME(IDX3)(1:4)
             IF  SKT1310TMP-KOHNAME(WRK-CNT)  NOT =  SPACE
               MOVE  SKT1310TMP-KOHNAME(WRK-CNT)
                                       TO  SKT1320-K-KOHNAME(IDX3)(5:)
             ELSE
               MOVE  HKN-TANSEIDONAME  TO  SKT1320-K-KOHNAME(IDX3)(5:)
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
      *    帳票印刷処理(入院用)
      *****************************************************************
       331-PRINT-OUT-SEC                SECTION.
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
           MOVE   "SKT1320.red"        TO  SPRT-PRTID
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
           MOVE    SKT1320             TO  SPRT-PRTDATA
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
      *    公費ソート処理
      *****************************************************************
       KOH-SORT-SEC          SECTION.
      *
      *
           INITIALIZE  SORT-AREA
      *    公費数を取得
           MOVE    ZERO    TO    IDX-MAX
                                 IDX2
           MOVE    SPACE   TO    WRK-KOHNUM
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >  20
             IF  SKT1310TMP-KOHNUM(IDX1)  =  SPACE
               MOVE  20    TO    IDX1
             ELSE
               IF    WRK-KOHNUM               =   SPACE   OR
                     SKT1310TMP-KOHNUM(IDX1)  <   WRK-KOHNUM
                 MOVE     IDX1                TO  IDX2
                 MOVE     SKT1310TMP-KOHNUM(IDX1) TO  WRK-KOHNUM
               END-IF
               ADD   1     TO    IDX-MAX
             END-IF
           END-PERFORM
           IF     IDX2     >     ZERO
             MOVE    SKT1310TMP-KOH-TBL(IDX2) TO  SORT-KOH-TBL(1)
           END-IF

           DISPLAY "一般ソート前１" SKT1310TMP-KOHNUM(1)
           DISPLAY "一般ソート前２" SKT1310TMP-KOHNUM(2)
           DISPLAY "一般ソート前３" SKT1310TMP-KOHNUM(3)
      *
           IF      IDX-MAX    >     ZERO
             MOVE    2    TO    IDX1
             MOVE    1    TO    IDX2
             MOVE    1    TO    IDX3
             MOVE   "999"   TO    WRK-KOHNUM
             PERFORM    UNTIL    IDX1  >    IDX-MAX
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >    IDX-MAX
                 IF  SKT1310TMP-KOHNUM(IDX2)  >   SORT-KOHNUM(IDX1 - 1)
                   IF  SKT1310TMP-KOHNUM(IDX2)  <  WRK-KOHNUM
                     MOVE  SKT1310TMP-KOHNUM(IDX2)  TO WRK-KOHNUM
                     MOVE  IDX2          TO    IDX3
                   END-IF
                 END-IF
               END-PERFORM
               MOVE  SKT1310TMP-KOH-TBL(IDX3)  TO  SORT-KOH-TBL(IDX1)
               MOVE   "999"   TO    WRK-KOHNUM
               ADD   1    TO    IDX1
             END-PERFORM
           END-IF
      *
           INITIALIZE             SKT1310TMP-KOH-AREA
           MOVE  SORT-AREA  TO    SKT1310TMP-KOH-AREA
      *
           DISPLAY "一般ソート後１" SKT1310TMP-KOHNUM(1)
           DISPLAY "一般ソート後２" SKT1310TMP-KOHNUM(2)
           DISPLAY "一般ソート後３" SKT1310TMP-KOHNUM(3)
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
                   MOVE    LNK-PRTKANRI-SRYYM
                                           TO  RECE10-SKYYM
                   MOVE    LNK-PRTKANRI-SKYYMD
                                           TO  RECE10-UPDYMD
      *
                   MOVE    RECE10-REC      TO  MCPDATA-REC
                   MOVE    "DBUPDATE"      TO  MCP-FUNC
                   MOVE    "tbl_seikyu"     TO  MCP-TABLE
                   MOVE    "key"            TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"      USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
                   IF      MCP-RC          =   ZERO
                       PERFORM 900-UPD-FILE-READ-SEC
                   ELSE
                       DISPLAY "UPD ERR KEY="  RECE10-KEY
                       MOVE  "*** SOKATU1310 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY
                   MOVE  "*** SOKATU1310 SEIKYU INVALID ***"
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
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE    SPA-HOSPNUM          TO  HKNJA-HOSPNUM
           MOVE    WRK-HKNJANUM         TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hknjainf"       TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    "tbl_hknjainf"   TO  MCP-TABLE
               MOVE    "key"            TO  MCP-PATHNAME
               PERFORM  925-HKNJAINF-READ-SEC
               MOVE  HKNJA-HKNJANAME   TO  WRK-HKNJANAME
           ELSE
               DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE  "tbl_hknjainf"         TO  MCP-TABLE
           MOVE  "key"                  TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"            USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       920-HKNJAINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者情報読込
      *****************************************************************
       925-HKNJAINF-READ-SEC  SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNJAINF-REC
             MOVE  ZERO         TO  FLG-HKNJAINF
           ELSE
             INITIALIZE  HKNJAINF-REC
             MOVE  1  TO  FLG-HKNJAINF
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
           MOVE    SPA-HOSPNUM          TO  HKN-HOSPNUM
           MOVE    WRK-KOHNUM           TO  HKN-HKNNUM
           MOVE    LNK-PRTKANRI-SRYYM   TO  HKN-TEKSTYMD(1:6)
           MOVE    "01"                 TO  HKN-TEKSTYMD(7:2)
           MOVE    HKN-TEKSTYMD         TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_hknnum"         TO  MCP-TABLE
           MOVE    "key5"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
               MOVE    "tbl_hknnum"     TO  MCP-TABLE
               MOVE    "key5"           TO  MCP-PATHNAME
               PERFORM  955-HKNNUM-READ-SEC
           ELSE
               DISPLAY  "HKNNUM SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
           MOVE  "tbl_hknnum"           TO  MCP-TABLE
           MOVE  "key5"                 TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"            USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       950-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号情報読込
      *****************************************************************
       955-HKNNUM-READ-SEC  SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL  "ORCDBMAIN"           USING
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
      *
      *****************************************************************
      *  診療情報セット
      *****************************************************************
       960-SRYACT-SET-SEC  SECTION.
      *
             INITIALIZE  SRYACT-REC
             MOVE  SPA-HOSPNUM           TO  SRY-HOSPNUM
             MOVE  RECE10-NYUGAIKBN      TO  SRY-NYUGAIKBN
             MOVE  RECE10-PTID           TO  SRY-PTID
             MOVE  RECE10-SRYYM          TO  SRY-SRYYM
             MOVE  SRYACT-REC            TO  MCPDATA-REC
             MOVE  "DBSELECT"            TO  MCP-FUNC
             MOVE  "tbl_sryact"          TO  MCP-TABLE
             MOVE  "key4"                TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
             IF  MCP-RC  =  ZERO
               MOVE  "tbl_sryact"        TO  MCP-TABLE
               MOVE  "key4"              TO  MCP-PATHNAME
               PERFORM  965-SRYACT-READ-SEC
      *    在総診の患者取得
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
             MOVE  "DBCLOSECURSOR"       TO  MCP-FUNC
             MOVE  "tbl_sryact"          TO  MCP-TABLE
             MOVE  "key4"                TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
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
           MOVE  "DBFETCH"      TO  MCP-FUNC
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  SRYACT-REC
             MOVE  ZERO         TO  FLG-SRYACT
           ELSE
             INITIALIZE   SRYACT-REC
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
               IF  SRY-SRYCD(IDX4)  =  WRK-CONS-ZAITAKU1
                                   OR  WRK-CONS-ZAITAKU2
                                   OR  WRK-CONS-ZAITAKU3
                                   OR  WRK-CONS-ZAITAKU4
                                   OR  WRK-CONS-ZAITAKU5
                                   OR  WRK-CONS-ZAITAKU6
                                   OR  WRK-CONS-ZAITAKU7
                                   OR  WRK-CONS-ZAITAKU8
                                   OR  WRK-CONS-ZAITAKU9
                                   OR  WRK-CONS-ZAITAKU10
                                   OR  WRK-CONS-ZAITAKU11
                                   OR  WRK-CONS-ZAITAKU12
                                   OR  WRK-CONS-ZAITAKU13
                                   OR  WRK-CONS-ZAITAKU14
                                   OR  WRK-CONS-ZAITAKU15
                                   OR  WRK-CONS-ZAITAKU16
                                   OR  WRK-CONS-ZAITAKU17
                                   OR  WRK-CONS-ZAITAKU18
                                   OR  WRK-CONS-ZAITAKU19
                                   OR  WRK-CONS-ZAITAKU20
                                   OR  WRK-CONS-ZAITAKU21
                                   OR  WRK-CONS-ZAITAKU22
                                   OR  WRK-CONS-ZAITAKU23
                                   OR  WRK-CONS-ZAITAKU24
                                   OR  WRK-CONS-ZAITAKU25
                                   OR  WRK-CONS-ZAITAKU26
                                   OR  WRK-CONS-ZAITAKU27
                                   OR  WRK-CONS-ZAITAKU28
                                   OR  WRK-CONS-ZAITAKU29
                                   OR  WRK-CONS-ZAITAKU30
                                   OR  WRK-CONS-ZAITAKU31
                                   OR  WRK-CONS-ZAITAKU32
                                   OR  WRK-CONS-ZAITAKU33
                                   OR  WRK-CONS-ZAITAKU34
                                   OR  WRK-CONS-ZAITAKU35
                                   OR  WRK-CONS-ZAITAKU36
                                   OR  WRK-CONS-ZAITAKU37
                                   OR  WRK-CONS-ZAITAKU38
                                   OR  WRK-CONS-ZAITAKU39
                                   OR  WRK-CONS-ZAITAKU40
                                   OR  WRK-CONS-ZAITAKU41
                                   OR  WRK-CONS-ZAITAKU42
                                   OR  WRK-CONS-ZAITAKU43
                                   OR  WRK-CONS-ZAITAKU44
                                   OR  WRK-CONS-ZAITAKU45
                                   OR  WRK-CONS-ZAITAKU46
                                   OR  WRK-CONS-ZAITAKU47
                                   OR  WRK-CONS-ZAITAKU48
                                   OR  WRK-CONS-ZAITAKU49
                                   OR  WRK-CONS-ZAITAKU50
                                   OR  WRK-CONS-ZAITAKU51
                                   OR  WRK-CONS-ZAITAKU52
                                   OR  WRK-CONS-ZAITAKU53
                                   OR  WRK-CONS-ZAITAKU54
                                   OR  WRK-CONS-ZAITAKU55
                                   OR  WRK-CONS-ZAITAKU56
                                   OR  WRK-CONS-ZAITAKU57
                                   OR  WRK-CONS-ZAITAKU58
                                   OR  WRK-CONS-ZAITAKU59
                                   OR  WRK-CONS-ZAITAKU60
                                   OR  WRK-CONS-ZAITAKU61
                                   OR  WRK-CONS-ZAITAKU62
                                   OR  WRK-CONS-ZAITAKU63
                                   OR  WRK-CONS-ZAITAKU64
                                   OR  WRK-CONS-ZAITAKU65
                                   OR  WRK-CONS-ZAITAKU66
                                   OR  WRK-CONS-ZAITAKU67
                                   OR  WRK-CONS-ZAITAKU68
                                   OR  WRK-CONS-ZAITAKU69
                                   OR  WRK-CONS-ZAITAKU70
                                   OR  WRK-CONS-ZAITAKU71
                                   OR  WRK-CONS-ZAITAKU72
                                   OR  WRK-CONS-ZAITAKU73
                                   OR  WRK-CONS-ZAITAKU74
                                   OR  WRK-CONS-ZAITAKU75
                                   OR  WRK-CONS-ZAITAKU76
                                   OR  WRK-CONS-ZAITAKU77
                                   OR  WRK-CONS-ZAITAKU78
                                   OR  WRK-CONS-ZAITAKU79
                                   OR  WRK-CONS-ZAITAKU80
                                   OR  WRK-CONS-ZAITAKU81
                                   OR  WRK-CONS-ZAITAKU82
                                   OR  WRK-CONS-ZAITAKU83
                                   OR  WRK-CONS-ZAITAKU84
                                   OR  WRK-CONS-ZAITAKU85
                                   OR  WRK-CONS-ZAITAKU86
                                   OR  WRK-CONS-ZAITAKU87
                                   OR  WRK-CONS-ZAITAKU88
                                   OR  WRK-CONS-ZAITAKU89
                                   OR  WRK-CONS-ZAITAKU90
                                   OR  WRK-CONS-ZAITAKU91
                                   OR  WRK-CONS-ZAITAKU92
                                   OR  WRK-CONS-ZAITAKU93
                                   OR  WRK-CONS-ZAITAKU94
                                   OR  WRK-CONS-ZAITAKU95
                                   OR  WRK-CONS-ZAITAKU96
                                   OR  WRK-CONS-ZAITAKU97
                                   OR  WRK-CONS-ZAITAKU98
                                   OR  WRK-CONS-ZAITAKU99
                                   OR  WRK-CONS-ZAITAKU100
                                   OR  WRK-CONS-ZAITAKU101
                                   OR  WRK-CONS-ZAITAKU102
                                   OR  WRK-CONS-ZAITAKU103
                                   OR  WRK-CONS-ZAITAKU104
                                   OR  WRK-CONS-ZAITAKU105
                                   OR  WRK-CONS-ZAITAKU106
                                   OR  WRK-CONS-ZAITAKU107
                                   OR  WRK-CONS-ZAITAKU108
                                   OR  WRK-CONS-ZAITAKU109
                                   OR  WRK-CONS-ZAITAKU110
                                   OR  WRK-CONS-ZAITAKU111
                                   OR  WRK-CONS-ZAITAKU112
                                   OR  WRK-CONS-ZAITAKU113
                                   OR  WRK-CONS-ZAITAKU114
                                   OR  WRK-CONS-ZAITAKU115
                                   OR  WRK-CONS-ZAITAKU116
                                   OR  WRK-CONS-ZAITAKU117
                                   OR  WRK-CONS-ZAITAKU118
                                   OR  WRK-CONS-ZAITAKU119
                                   OR  WRK-CONS-ZAITAKU120
                                   OR  WRK-CONS-ZAITAKU121
                                   OR  WRK-CONS-ZAITAKU122
                                   OR  WRK-CONS-ZAITAKU123
                                   OR  WRK-CONS-ZAITAKU124
                                   OR  WRK-CONS-ZAITAKU125
                                   OR  WRK-CONS-ZAITAKU126
                                   OR  WRK-CONS-ZAITAKU127
                                   OR  WRK-CONS-ZAITAKU128
                                   OR  WRK-CONS-ZAITAKU129
                                   OR  WRK-CONS-ZAITAKU130
                                   OR  WRK-CONS-ZAITAKU131
                                   OR  WRK-CONS-ZAITAKU132
                                   OR  WRK-CONS-ZAITAKU133
      *
                 MOVE  "1"  TO  SKT1310TMP-ZAITAKU
               END-IF
             END-IF
           END-PERFORM
      *
           .
       965-ZAISOU-CHK-EXT.
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
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
      *
           MOVE    SPA-HOSPNUM          TO  RECE10-HOSPNUM
           MOVE   LNK-PRTKANRI-SRYYM    TO    RECE10-SKYYM.
      *
           IF    WRK-PARA-HKNJANUM  NOT = ZERO
               MOVE    WRK-PARA-HKNJANUM  TO  RECE10-XXSTHKNJANUM
                                              RECE10-XXEDHKNJANUM
           ELSE
               MOVE    SPACE            TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"       TO  RECE10-XXEDHKNJANUM
           END-IF
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  000000               TO  RECE10-SRYYM
             MOVE  999999               TO  RECE10-XXSRYYM
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
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE10-REC
      *
      *        返戻分でオンライン請求は読み飛ばす
               IF      (WRK-PARA-SYOKBN         =  "2")
               AND     (RECE10-HENREI-SIJI-FLG  =  1)
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
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
      *    都内のデータは対象外
               IF    RECE10-PREFKBN   =  "1"  OR
                     RECE10-PREFNUM   =  "13"
                 GO    TO    900-SEIKYU-READ-SEC
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
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
               MOVE    "tbl_ptinf"       TO  MCP-TABLE
               MOVE    "key"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"       USING
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
           MOVE    "tbl_ptinf"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
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
           READ    SKT1310TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT1310TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT1310TMP
           END-READ
           .
       900-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  SKT1310TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
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
      *
           MOVE    "DBSELECT"                   TO  MCP-FUNC
           MOVE    "tbl_syskanri"            TO  MCP-TABLE
           MOVE    "key10"                   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"               USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                    TO  MCP-FUNC
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
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
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
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
           MOVE    "DBSELECT"                   TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                TO  MCP-FUNC
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
      *
           MOVE    "DBCLOSECURSOR"                    TO  MCP-FUNC
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
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1900-STYUKYMD(1:6)
           END-IF
           MOVE    "01"                    TO  SYS-1900-STYUKYMD (7:2)
           MOVE    SYS-1900-STYUKYMD       TO  SYS-1900-EDYUKYMD
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
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
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1901-STYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1901-STYUKYMD (7:2)
           MOVE    SYS-1901-STYUKYMD   TO  SYS-1901-EDYUKYMD
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
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1902-STYUKYMD(1:6)
           END-IF
           MOVE    "01"                TO  SYS-1902-STYUKYMD (7:2)
           MOVE    SYS-1902-STYUKYMD   TO  SYS-1902-EDYUKYMD
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
       900-DBDISCONNECT-EXT.
           EXIT.
