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
       PROGRAM-ID.             SOKATU1320.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : レセ電用国保総括表（東京）用集計処理
      *  管理者            :
      *  作成日付    作業者        記述
      *  08/06/20    吉川          新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    内田    08/08/19    在総診の診療があれば別集計とする
      *  01.00.02    吉川    09/01/26    食事が無く生活療養のみの場合は
      *                                  金額を集計しない
      *                                  在宅でも患者負担金が無い場合は
      *                                  通常請求分に集計する
      *  01.00.03    吉川    2009/02/25  09/01/26に変更した在総診の
      *                                  集計方法を元に戻す
      *  01.00.04    吉川    2009/03/27  70歳未満と70歳以上の在宅が
      *                                  別れて印字される不具合を修正
      *  01.00.05    吉川    2010/08/03  オンライン返戻対応
      *                                  特別療養費国保対応
      *  01.00.06    吉川    2010/10/08  再印刷時の帳票タイトル修正
      *  01.00.07    吉川    2010/12/15  特別療養費国保(068)が退職者に
      *                                  記載される不具合を修正
      *                                  open-cobol1.0対応
      *  01.00.08    吉川    2012/05/16  H24年4月に新設された在総診対応
      *  01.00.09    吉川    2012/12/21  医療機関情報編集対応
      *  01.00.10    吉川    2013/07/26  在宅の対象とする診療コード追加
      *  01.00.11    萩野    2014/06/06  在宅の対象とする診療コード追加
      *  01.00.12    吉川    2014/10/08  ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.13    萩野    2014/12/05  返戻時、再印刷画面のタイトル修正
      *  01.00.14    門間    2016/03/08  医療機関編集情報への開設者名追加対応
      *  01.00.13    萩野    2016/05/12  在宅の対象とする診療コード追加
      *  01.00.14    門間    2018/01/09  医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *                                  SITEKBN変更（動作に変更なし）
      *  01.00.15    門間    2018/06/11  記述漏れ対応
      *  01.00.16    門間    2018/11/02  修正履歴コメント削除
      *  01.00.17    門間    2019/09/02  開設者氏名の印字文字数の増加
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.

      *    総括表一時ファイル
           SELECT  SKT1320TMP-FILE ASSIGN    SKT1320TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT1320TMP-KEY
                                   FILE    STATUS  IS  STS-SKT1320.
      *
      *    請求ＤＢ更新用ファイル
           SELECT  UPD-FILE        ASSIGN  RECEUPD
                                   FILE    STATUS  IS  STS-RECEUPD.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    総括表一時ファイル
       FD  SKT1320TMP-FILE.
           COPY    "CPSKT1320TMP.INC".
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
      *    印刷用データ 名称領域
           COPY    "CPCOMMONPRT.INC".
      *
      *    総括表一時ファイル 名称領域
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //SKT1320TP//.
      *
      *    請求ＤＢ更新用ファイル 名称領域
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
      *    印刷用領域
           COPY    "SKT1330.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT1320         PIC X(02).
           03  STS-RECE10          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT1320TMP      PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-KOUKI           PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-KANJAFTN        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
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
           03  WRK-SEIYMDWH        PIC X(20).
           03  WRK-SRYYMWH         PIC X(16).
           03  WRK-SRYYMD          PIC X(08).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-Z5              PIC  Z,ZZZ.
           03  WRK-Z14             PIC  ZZ,ZZZ,ZZZ,ZZZ.
      *
      *    文字変換
           03  WRK-MAE-INPUT       PIC X(200).
           03  WRK-OUT-INPUT       PIC X(200).
      *
           03  WRK-PATH                PIC X(50).
           03  WRK-PATH2               PIC X(50).
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-PATH3               PIC X(50).
           03  WRK-SEIKYU              PIC 9(07).
           03  WRK-KOHNUM              PIC X(03).
      *
      *    医療機関情報
           03  WRK-HOSPNAME            PIC X(100).
           03  WRK-HOSPNAME-R          REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1       PIC X(50).
               05  WRK-HOSPNAME2       PIC X(50).
           03  WRK-HOSPADRS            PIC X(100).
           03  WRK-HOSPADRS-R          REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1       PIC X(50).
               05  WRK-HOSPADRS2       PIC X(50).
           03  WRK-HOSPTEL             PIC X(15).
           03  WRK-KAISETUNAME         PIC X(80).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
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
      *    ＪＯＢ管理ＤＢ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ＪＯＢ管理ＤＢ（制御用）
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
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
      *    請求管理情報
           COPY    "CPRCF010.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      ******************************************************************
      *    サブプロ用  領域
      *****************************************************************
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
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
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
      *    主    処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           DISPLAY "*** SOKATU1320 START ***"
      *
           PERFORM  100-INIT-SEC
      *
           PERFORM  200-MAIN-SEC
      *
           PERFORM  300-END-SEC
      *
           STOP     RUN
           .
      *
      *****************************************************************
      *    初期  処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                   FLG-AREA
           INITIALIZE                   STS-AREA
           INITIALIZE                   WRK-AREA
           INITIALIZE                   CNT-AREA
           INITIALIZE                   IDX-AREA
           INITIALIZE                   SPA-AREA
           INITIALIZE                   RECEERR
      *
           ACCEPT     SYS-TIME          FROM    TIME
      *
           PERFORM    100-DBOPEN-SEC
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
      *    ステップ開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SOKATU1320"    TO  JOB-PGID
           MOVE    "レセ電用国保総括表"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "SKT1320"         TO  SKT1320TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT1320TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT1320TP-HOSPNUM
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID     TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  SKT1320TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT1320TP-FULLNAME
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
           INITIALIZE                      SYS-1001-REC
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
           MOVE    SYS-1001-REC   TO      MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF     FLG-SYSKANRI        =   ZERO
                  MOVE    MCPDATA-REC     TO    SYS-1001-REC
                  IF      SYS-1001-HOSPNAME  =  LOW-VALUE
                          MOVE    SPACE   TO    SYS-1001-HOSPNAME
                  END-IF
                  IF      SYS-1001-KAISETUNAME  =   LOW-VALUE
                          MOVE    SPACE   TO    SYS-1001-KAISETUNAME
                  END-IF
           ELSE
                  MOVE    "医療機関情報が取得できませんでした"
                                          TO    WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    1               TO    FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE                     SYS-1002-REC
           MOVE     "1002"             TO  SYS-1002-KANRICD
           MOVE     "*"                TO  SYS-1002-KBNCD
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
           MOVE     SYS-1002-REC  TO      MCPDATA-REC
           PERFORM  900-SYSKANRI-INV-SEC
           IF       FLG-SYSKANRI       =  ZERO
                    MOVE    MCPDATA-REC   TO    SYS-1002-REC
                    IF      SYS-1002-ADRS =     LOW-VALUE
                            MOVE    SPACE TO    SYS-1002-ADRS
                    END-IF
           ELSE
                    MOVE "医療機関情報（所在地）が取得できませんでした"
                                          TO    WRK-RECEERR
                    PERFORM   500-ERR-HENSYU-SEC
                    MOVE      1           TO    FLG-END
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
           OPEN     OUTPUT  UPD-FILE
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
           PERFORM  900-SEIKYU-SELECT-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主    処理
      *****************************************************************
       200-MAIN-SEC               SECTION.
      *
           OPEN     OUTPUT  SKT1320TMP-FILE
           CLOSE            SKT1320TMP-FILE
           OPEN     I-O     SKT1320TMP-FILE
      *
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           PERFORM  900-SEIKYU-CLOSE-SEC
           IF  WRK-PARA-TOKUBETU  =  "1"
             MOVE     ZERO    TO        FLG-END
             MOVE     1       TO        FLG-KOUKI
             MOVE    "039"    TO        WRK-HKNNUM
             PERFORM  950-SEIKYU-SELECT-SEC
             PERFORM  2001-KOKUHO-SYUKEI-SEC
                      UNTIL   FLG-END   =   1
             PERFORM  950-SEIKYU-CLOSE-SEC
           ELSE
             MOVE     1       TO        FLG-KOUKI
             MOVE     ZERO    TO        FLG-END
             MOVE    "040"    TO        WRK-HKNNUM
             PERFORM  950-SEIKYU-SELECT-SEC
             PERFORM  2001-KOKUHO-SYUKEI-SEC
                      UNTIL   FLG-END   =   1
             PERFORM  950-SEIKYU-CLOSE-SEC
           END-IF
      *
           CLOSE   SKT1320TMP-FILE
           CLOSE   UPD-FILE
      *
           IF       FLG-UPD   =       1
                    PERFORM   2002-PRINT-SEC
           END-IF
      *
           .
      *
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
                           PERFORM     3001-SEIKYU-UPDATE-SEC
                   END-IF
           END-IF
      *
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SOKATU1320 IN  "  CNT-RECE10
           DISPLAY "*** SOKATU1320 END ***"
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
       2001-KOKUHO-SYUKEI-SEC               SECTION.
      *
      *TMP-FILE キーセット
           INITIALIZE       SKT1320TMP-REC
      *
      *    在宅→外来→入院の順で印刷
           IF      RECE10-ZAITAKU      =   "1"
               MOVE    1               TO  SKT1320TMP-SYUTURYOKU
           ELSE
               IF      RECE10-NYUGAIKBN    =   "2"
                   MOVE    2           TO  SKT1320TMP-SYUTURYOKU
               ELSE
                   MOVE    3           TO  SKT1320TMP-SYUTURYOKU
               END-IF
           END-IF
      *
           MOVE    RECE10-ZAITAKU      TO  SKT1320TMP-ZAITAKU
      *
           MOVE    RECE10-NYUGAIKBN    TO  SKT1320TMP-NYUGAIKBN
      *
      *    在総診・在医総か
           IF  RECE10-ZAITAKU  =  SPACE
             PERFORM  960-SRYACT-SET-SEC
           END-IF
      *
           PERFORM  900-SKTTMP-READ-SEC
           IF  FLG-KOUKI  =  ZERO
               PERFORM  220-HKN-SYUKEI-SEC
           ELSE
               PERFORM  220-KOUKI-SYUKEI-SEC
           END-IF
      *
      *    集計の対象にならなかったとき
           IF     FLG-UPD        =       1
                  PERFORM   240-UPD-FILE-HENSYU-SEC
      *
               IF  FLG-KOUKI  =  ZERO
                  PERFORM   900-SEIKYU-READ-SEC
               ELSE
                  PERFORM   950-SEIKYU-READ-SEC
               END-IF
           ELSE
                  MOVE      SPACE TO      WRK-RECEERR
                  STRING    "集計対象外  患者番号＝"
                                               DELIMITED  BY  SIZE
                            RECE10-PTNUM       DELIMITED  BY  SPACE
                            " 診療年月＝"      DELIMITED  BY  SIZE
                            RECE10-SRYYM       DELIMITED  BY  SIZE
                            " レセ種別＝"      DELIMITED  BY  SIZE
                            RECE10-RECESYUBETU DELIMITED  BY  SIZE
                         INTO                  WRK-RECEERR
                  END-STRING
                  PERFORM   500-ERR-HENSYU-SEC
                  MOVE      1         TO        FLG-END
           END-IF
      *
           .
      *
       2001-KOKUHO-SYUKEI-EXT.
           EXIT.
      *****************************************************************
      *    一般被保険者・退職者(本人・家族)・老人保健分集計処理
      *****************************************************************
       220-HKN-SYUKEI-SEC               SECTION.
      *
      *都内／都外判定
           IF      RECE10-PREFNUM      =   "13"
               MOVE    1               TO  IDX1
           ELSE
               MOVE    2               TO  IDX1
           END-IF
      *
      *保険判定
      *老人保健
           IF      RECE10-RJNHKNNUM    NOT =   SPACE
               MOVE    3               TO  IDX2
           ELSE
               IF      RECE10-HKNNUM   =   "060" OR "068"
                   MOVE    1           TO  IDX2
               ELSE
                   MOVE    2           TO  IDX2
               END-IF
           END-IF
      *
      *    件数
           ADD     1                   TO  SKT1320TMP-KENSU(IDX1 IDX2)
                                           SKT1320TMP-KENSU(IDX1 4)
      *
      *    点数
           ADD     RECE10-TOTALTEN(1)  TO  SKT1320TMP-TENSU(IDX1 IDX2)
                                           SKT1320TMP-TENSU(IDX1 4)
      *
      *    食事件数
           IF      RECE10-SHOKUJINISSU(1)  NOT =   ZERO
               ADD     1               TO  SKT1320TMP-SKENSU(IDX1 IDX2)
                                           SKT1320TMP-SKENSU(IDX1 4)
           END-IF
      *
      *    金額
           ADD     RECE10-SHOKUJIRYOYOHI(1)
                                       TO  SKT1320TMP-SKINGK(IDX1 IDX2)
                                           SKT1320TMP-SKINGK(IDX1 4)
      *
      *    公費件数
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       4
                   OR      RECE10-KOHNUM(IDX3) =   SPACE
               ADD     1       TO  SKT1320TMP-KKENSU(IDX1)
           END-PERFORM
      *
           IF      FLG-SKT1320TMP   =          1
               WRITE    SKT1320TMP-REC
           ELSE
               REWRITE  SKT1320TMP-REC
           END-IF

           MOVE    1               TO  FLG-UPD
           .
       220-HKN-SYUKEI-EXT.
           EXIT.
      *****************************************************************
      *    後期高齢者分集計処理
      *****************************************************************
       220-KOUKI-SYUKEI-SEC               SECTION.
      *
      *都内／都外判定
           IF      RECE10-PREFNUM      =   "13"
               MOVE    1               TO  IDX1
           ELSE
               MOVE    2               TO  IDX1
           END-IF
      *
      *    件数
           ADD     1                   TO  SKT1320TMP-KO-KENSU(IDX1)
      *    点数
           ADD     RECE10-TOTALTEN(1)  TO  SKT1320TMP-KO-TENSU(IDX1)
      *    食事件数
           IF      RECE10-SHOKUJINISSU(1)  NOT =   ZERO
               ADD     1               TO  SKT1320TMP-KO-SKENSU(IDX1)
      *    金額
               ADD     RECE10-SHOKUJIRYOYOHI(1)
                                       TO  SKT1320TMP-KO-SKINGK(IDX1)
           END-IF
      *    公費件数
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       4
                   OR      RECE10-KOHNUM(IDX3) =   SPACE
               ADD     1               TO  SKT1320TMP-KO-KKENSU(IDX1)
           END-PERFORM
      *
           IF      FLG-SKT1320TMP   =      1
               WRITE    SKT1320TMP-REC
           ELSE
               REWRITE  SKT1320TMP-REC
           END-IF
      *
           MOVE    1               TO  FLG-UPD
           .
       220-KOUKI-SYUKEI-EXT.
           EXIT.
      *****************************************************************
      *    請求ＤＢ更新用ファイル出力処理
      *****************************************************************
       240-UPD-FILE-HENSYU-SEC         SECTION.
      *
      *    集計の対象にならなかったとき
           IF     FLG-UPD   NOT   =       1
                  MOVE      SPACE TO      WRK-RECEERR
                  STRING    "集計対象外  患者番号＝"
                                               DELIMITED  BY  SIZE
                            RECE10-PTNUM       DELIMITED  BY  SPACE
                            " 診療年月＝"      DELIMITED  BY  SIZE
                            RECE10-SRYYM       DELIMITED  BY  SIZE
                            " レセ種別＝"      DELIMITED  BY  SIZE
                            RECE10-RECESYUBETU DELIMITED  BY  SIZE
                         INTO                  WRK-RECEERR
                  END-STRING
                  PERFORM   500-ERR-HENSYU-SEC
                  MOVE      1         TO        FLG-END
                  GO        TO        240-UPD-FILE-HENSYU-EXT
           END-IF
      *
           MOVE   RECE10-REC          TO        RECE10X-REC
           WRITE  RECE10X-REC
      *
           ADD    1                   TO        CNT-UPDFILE
           .
      *
       240-UPD-FILE-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    帳票ファイル出力処理
      *****************************************************************
       2002-PRINT-SEC                          SECTION.
      *
           INITIALIZE   SKT1320TMP-REC
           OPEN     INPUT       SKT1320TMP-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =   1
      *
      *    帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
      *    帳票印刷処理
               PERFORM 330-PRINT-OUT-SEC
      *    一時ファイル読込
               PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE    SKT1320TMP-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC                 SECTION.
      *
           ADD  1  TO  CNT-PAGE
           INITIALIZE                      SKT1330
      *    総括表発行日
           MOVE    WRK-SRYYMWH         TO  SKT1330-SEIYM
      *    医療機関住所
           MOVE    WRK-HOSPADRS        TO  SKT1330-ADRS
           INSPECT SKT1330-ADRS        REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME        TO  SKT1330-HOSPNAME
           INSPECT SKT1330-HOSPNAME    REPLACING ALL "  " BY "　"
      *    開設者氏名
           IF  WRK-KAISETUNAME(41:)  =  SPACE
               MOVE    WRK-KAISETUNAME     TO  SKT1330-KAISETUNAME
           ELSE
               MOVE    WRK-KAISETUNAME     TO  SKT1330-KAISETUNAME2
           END-IF
      *    電話番号
           MOVE    WRK-HOSPTEL         TO  SKT1330-TEL
      *    郵便番号
           MOVE    SYS-1002-POST       TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT(1:6)  TO  SKT1330-ZIP1
           MOVE    WRK-OUT-INPUT(7:2)  TO  SKT1330-ZIP2
           MOVE    WRK-OUT-INPUT(9:2)  TO  SKT1330-ZIP3
           MOVE    WRK-OUT-INPUT(11:2) TO  SKT1330-ZIP4
           MOVE    WRK-OUT-INPUT(13:2) TO  SKT1330-ZIP5
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN    TO  SKT1330-HOSPCD
      *
      *    在宅
           IF  SKT1320TMP-ZAITAKU     =     "1"
             MOVE  "在"                TO  SKT1330-ZAI1
             MOVE  "◯"                TO  SKT1330-ZAI2
           END-IF
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"        TO  SKT1330-TOKUBETU
           END-IF
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC               SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       2
      *
      *    公費件数
               MOVE    SKT1320TMP-KKENSU(IDX1)     TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT1330-HEIYOKEN(IDX1)
      *
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       4
      *
      *        件数
                   MOVE    SKT1320TMP-KENSU(IDX1 IDX2) TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT1330-KENSU(IDX1 IDX2)
      *
      *        点数
                   MOVE    SKT1320TMP-TENSU(IDX1 IDX2) TO  WRK-Z14
                   MOVE    WRK-Z14     TO  SKT1330-TENSU(IDX1 IDX2)
      *
      *        食事件数
                   MOVE    SKT1320TMP-SKENSU(IDX1 IDX2)
                                                       TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT1330-SKENSU(IDX1 IDX2)
      *
      *        食事金額
                   MOVE    SKT1320TMP-SKINGK(IDX1 IDX2)
                                                       TO  WRK-Z14
                   MOVE    WRK-Z14     TO  SKT1330-SKINGK(IDX1 IDX2)
               END-PERFORM
      *        後期高齢者
      *        件数
               MOVE    SKT1320TMP-KO-KENSU(IDX1)   TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT1330-KKENSU(IDX1)
      *        点数
               MOVE    SKT1320TMP-KO-TENSU(IDX1)   TO  WRK-Z14
               MOVE    WRK-Z14     TO  SKT1330-KTENSU(IDX1)
      *        食事件数
               MOVE    SKT1320TMP-KO-SKENSU(IDX1)  TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT1330-KSKENSU(IDX1)
      *        食事金額
               MOVE    SKT1320TMP-KO-SKINGK(IDX1)  TO  WRK-Z14
               MOVE    WRK-Z14     TO  SKT1330-KSKINGK(IDX1)
      *        公費併用件数
               MOVE    SKT1320TMP-KO-KKENSU(IDX1)  TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT1330-KHEIYOKEN(IDX1)
           END-PERFORM
      *
          .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       330-PRINT-OUT-SEC                   SECTION.
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
           MOVE    "SKT1330.red"       TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "レセ電用国保総括表"                TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "レセ電用国保総括表［返戻分］"      TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "レセ電用国保総括表［特別療養費］"  TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "レセ電用国保総括表［返戻分］［特別療養費］"
                                                         TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT1330             TO  SPRT-PRTDATA
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
      *
           .
       330-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC               SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR     =    ZERO
                   CONTINUE
           ELSE
               OPEN    OUTPUT           RECEERR-FILE
      *
               MOVE    WRK-RECEERR      TO   RECEERR-REC
               WRITE   RECEERR-REC
      *
      *        ジョブ終了処理
               MOVE    "JBE"            TO   SJOBKANRI-MODE
               INITIALIZE                    JOBKANRI-REC
               MOVE    SPA-HOSPNUM      TO   JOB-HOSPNUM
               MOVE    WRK-PARA-JOBID   TO   JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                        TO   JOB-SHELLID
               MOVE    WRK-RECEERR      TO   JOB-YOBI
               MOVE    "9999"           TO   JOB-ERRCD
               CALL    "ORCSJOB"        USING
                                        ORCSJOBKANRIAREA
                                        JOBKANRI-REC
                                        SPA-AREA
           END-IF
      *
           CLOSE   RECEERR-FILE
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *   請求管理ＤＢ更新処理
      *****************************************************************
       3001-SEIKYU-UPDATE-SEC              SECTION.
      *
           OPEN     INPUT           UPD-FILE
      *
           PERFORM  900-UPD-FILE-READ-SEC
      *
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
                       MOVE  "*** SOKATU1320 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY
                   MOVE  "*** SOKATU1320 SEIKYU INVALID ***"
                                           TO  WRK-RECEERR
                   MOVE    RECE10-KEY      TO  WRK-RECEERR(33:)
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1               TO  FLG-UPDFILE
               END-IF
           END-PERFORM
      *
           CLOSE   UPD-FILE
      *
           .
       3001-SEIKYU-UPDATE-EXT.
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
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "  "
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
           INITIALIZE         RECE10-REC.
           MOVE   SPA-HOSPNUM             TO  RECE10-HOSPNUM
           MOVE   LNK-PRTKANRI-SRYYM      TO  RECE10-SKYYM.
      *
           IF    WRK-PARA-HKNJANUM  NOT = ZERO
               MOVE    WRK-PARA-HKNJANUM  TO  RECE10-XXSTHKNJANUM
                                              RECE10-XXEDHKNJANUM
           ELSE
               MOVE    SPACE              TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"         TO  RECE10-XXEDHKNJANUM
           END-IF
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  000000                 TO  RECE10-SRYYM
             MOVE  999999                 TO  RECE10-XXSRYYM
           END-IF
      *
           MOVE    RECE10-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
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
               DISPLAY "SELECT ERR"
           END-IF
           .
      *
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
      *
               ADD     1                   TO  CNT-RECE10
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
           .
      *
       900-SEIKYU-READ-EXT.
           EXIT.
      *****************************************************************
      *    請求管理読込（後期高齢）
      *****************************************************************
       950-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE         RECE10-REC.
           MOVE   SPA-HOSPNUM             TO  RECE10-HOSPNUM
           MOVE   LNK-PRTKANRI-SRYYM      TO  RECE10-SKYYM
           MOVE   WRK-CONS-SRYYM-200804   TO  RECE10-SRYYM
           MOVE   999999                  TO  RECE10-XXSRYYM
           MOVE   WRK-HKNNUM              TO  RECE10-HKNNUM-S
                                              RECE10-HKNNUM
      *
           IF    WRK-PARA-HKNJANUM  NOT = ZERO
               MOVE    WRK-PARA-HKNJANUM  TO  RECE10-XXSTHKNJANUM
                                              RECE10-XXEDHKNJANUM
           ELSE
               MOVE    SPACE              TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"         TO  RECE10-XXEDHKNJANUM
           END-IF
      *
           MOVE    RECE10-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    WRK-PATH2               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               PERFORM 950-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
               DISPLAY "SELECT ERR"
           END-IF
           .
      *
       950-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込（後期高齢）
      *****************************************************************
       950-SEIKYU-READ-SEC           SECTION.
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-PATH2           TO  MCP-PATHNAME
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
                   GO  TO  950-SEIKYU-READ-SEC
               END-IF
      *
               IF      WRK-PARA-STSRYYM    =   "999999"
                   CONTINUE
               ELSE
                   IF  ( RECE10-SRYYM      <   WRK-PARA-STSRYYM )
                   OR  ( RECE10-SRYYM      >   WRK-PARA-EDSRYYM )
                       GO  TO  950-SEIKYU-READ-SEC
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
                   GO  TO  950-SEIKYU-READ-SEC
               END-IF
      *
               ADD     1                   TO  CNT-RECE10
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
           .
      *
       950-SEIKYU-READ-EXT.
           EXIT.
      *****************************************************************
      *    請求管理クローズ
      *****************************************************************
       900-SEIKYU-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    WRK-PATH             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    請求管理クローズ
      *****************************************************************
       950-SEIKYU-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    WRK-PATH2            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"               TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
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
      *    請求額集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           MOVE    ZERO                           TO  WRK-SEIKYU
      *
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     4  OR
                    SYU-KOH-HKNNUM(IDX3) =  SPACE
      *
             IF   SYU-KOH-HKNNUM(IDX3)   =  WRK-KOHNUM
               IF  RECE10-FTNMONEY(IDX3)  >  ZERO
                 MOVE  SYU-KOH-COMPFTN(IDX3)    TO  WRK-SEIKYU
               ELSE
                 MOVE  SYU-KOH-COMPFTN-ENTANI(IDX3)
                                                TO  WRK-SEIKYU
               END-IF
             END-IF
           END-PERFORM
      *
           IF  WRK-SEIKYU  =  ZERO
               MOVE  ZERO  TO  FLG-KANJAFTN
           END-IF
      *
           .
       800-SEIKYU-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *  収納情報セット
      *****************************************************************
       920-SYUNOU-SET-SEC  SECTION.
      *
           INITIALIZE  SYUNOU-REC
           MOVE  SPA-HOSPNUM        TO  SYU-HOSPNUM
           MOVE  RECE10-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE  RECE10-PTID        TO  SYU-PTID
           MOVE  RECE10-SRYYM(1:6)  TO  SYU-SRYYMD(1:6)
           MOVE  "__"               TO  SYU-SRYYMD(7:2)
           MOVE  WRK-KOHNUM         TO  SYU-KOH-HKNNUM(1)
                                        SYU-KOH-HKNNUM(2)
                                        SYU-KOH-HKNNUM(3)
                                        SYU-KOH-HKNNUM(4)
           MOVE  SYUNOU-REC         TO  MCPDATA-REC
           MOVE  "DBSELECT"         TO  MCP-FUNC
           MOVE  "tbl_syunou"       TO  MCP-TABLE
           MOVE  WRK-PATH3          TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"            USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_syunou"       TO  MCP-TABLE
             MOVE    WRK-PATH3          TO  MCP-PATHNAME
             PERFORM  925-SYUNOU-READ-SEC
      *
             PERFORM  UNTIL  FLG-SYUNOU  =  1
      *
               IF  SYU-DENPJTIKBN   NOT = "3"
                   PERFORM  800-SEIKYU-SEC
               END-IF
      *
               MOVE    "tbl_syunou"       TO  MCP-TABLE
               MOVE    WRK-PATH3          TO  MCP-PATHNAME
               PERFORM   925-SYUNOU-READ-SEC
             END-PERFORM
           ELSE
             DISPLAY  "SYUNOU SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
           MOVE    "tbl_syunou"       TO  MCP-TABLE
           MOVE    WRK-PATH3          TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *
           .
       920-SYUNOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納読込
      *****************************************************************
       925-SYUNOU-READ-SEC  SECTION.
      *
           MOVE  "DBFETCH"         TO  MCP-FUNC
           CALL    "ORCDBMAIN"        USING
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
       925-SYUNOU-READ-EXT.
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
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL   (IDX3  >     5) OR
                            (SRY-SRYCD(IDX3)  =  SPACE)
             IF  SRY-SRYKBN  =  "14"
               IF  SRY-SRYCD(IDX3)  =  WRK-CONS-ZAITAKU1
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
                 MOVE  1  TO  SKT1320TMP-SYUTURYOKU
                 MOVE  "1"  TO  SKT1320TMP-ZAITAKU
               END-IF
             END-IF
           END-PERFORM
      *
           .
       965-ZAISOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
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
           MOVE    "DBCLOSECURSOR"                    TO  MCP-FUNC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタ読込
      *****************************************************************
       910-SEIKYU-INV-SEC             SECTION.
      *
           MOVE    "DBSELECT"                 TO  MCP-FUNC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
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
      *    総括表一時ファイルＲＥＡＤ処理
      *****************************************************************
       900-SKTTMP-READ-SEC                 SECTION.
      *
           MOVE   ZERO        TO        FLG-SKT1320TMP
           READ   SKT1320TMP-FILE
                  INVALID     KEY
                              MOVE    1     TO      FLG-SKT1320TMP
           END-READ
           .
      *
       900-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  SKT1320TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
           END-READ
           .
       900-TMP-READ-EXT.
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
           PERFORM 900-SYSKANRI-INV-SEC
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
           PERFORM 900-SYSKANRI-INV-SEC
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
           PERFORM 900-SYSKANRI-INV-SEC
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
           PERFORM 900-SYSKANRI-INV-SEC
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
      *    ＤＢ  オープン処理
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
      *    ＤＢ  クローズ処理
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
