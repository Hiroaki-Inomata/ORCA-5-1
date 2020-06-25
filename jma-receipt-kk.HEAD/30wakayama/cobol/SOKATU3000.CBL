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
       PROGRAM-ID.             SOKATU3000.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 和歌山・診療報酬請求書(国保請求書)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/05/31    山口          ORCAのバージョンアップ(2.5.0)による再作成
      *                            (返戻対応含む)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01      内田     06/11/18    食事がない場合でも生活療養費を印字
      *　01.01.00　　　楠本　　07/04/02　　静岡版を和歌山版にカスタマイズ
      *  02.00.00      楠本　　07/10/11    ORCA4.0対応
      *  02.01.00      楠本    08/04/18    後期高齢　未就学児対応
      *  02.02.01      吉川    10/05/19    SPA-AREA初期化
      *                                    システム管理マスタ「1901」の
      *                                    医療機関情報を参照
      *                                    シス管「2007」の公費まとめ対応
      *                                    オンライン返戻対応
      *                                    在総診の診療があれば別集計とする
      *  02.02.02      吉川    10/12/08    特別療養費国保対応
      *                                    open-cobol1.0対応
      *  02.00.03      吉川    13/01/11    プラグイン対応
      *                                    医療機関情報編集対応
      *  02.00.04      吉川    14/01/08    在宅の対象とする診療コード追加
      *                                    プログラムオプション対応
      *  02.00.05      吉川    14/05/14    前期高齢者２割対応
      *                                    在宅の対象とする診療コード変更
      *  02.00.06      吉川    14/08/06    返戻時、再印刷画面のタイトル修正
      *  02.00.07      吉川    14/10/24    ver4.8.0対応（一時ディレクトリ変更）
      *  02.00.08      萩野    16/02/12    医療機関編集情報への開設者名追加対応
      *  02.00.09      門間    16/05/10    在宅の対象とする診療コード変更
      *  02.00.10      萩野    17/02/14    公費番号まとめ情報の公費番号の追加
      *                                    医療機関コード変更時、
      *                                    医療機関コード別の作成処理対応
      *  02.00.11      萩野    18/04/09    SITEKBN変更（動作に変更なし）
      *  02.00.12      萩野    18/12/12    修正履歴コメント削除
      *                                    保険番号情報の取得処理を修正
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.

      *    請求書一時ファイル  
           SELECT  SKT3000TMP-FILE ASSIGN    SKT3000TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT3000TMP-KEY
                                   FILE    STATUS  IS  STS-SKT3000.
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
       FD  SKT3000TMP-FILE.
           COPY    "CPSKT3000TMP.INC".
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
      *
       WORKING-STORAGE             SECTION.
      *
      *    請求書一時ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //SKT3000TP//.
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
           COPY    "SKT3000.INC".
           COPY    "SKT3010.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT3000         PIC X(02).
           03  STS-RECE10          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-YUSEN           PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT3000TMP      PIC 9(01).
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
           03  IDX                 PIC 9(05).
           03  IDY                 PIC 9(04).
      *
      *  パラメタ領域（地方公費仕様）
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
      *     03  WRK-PARA-HOSPID     PIC X(24).
           03  WRK-PARA-HOSPNUM    PIC 9(2).
           03  WRK-PARA-HKNJANUM   PIC X(08).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-TOKUBETU   PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).    
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
           03  WRK-CNT1            PIC 9(02).
           03  WRK-CNT2            PIC 9(02).
           03  WRK-KOHNUM          PIC X(03).
           03  WRK-HKNJANUM        PIC X(06).
           03  WRK-HKNJANAME       PIC X(20).
           03  WRK-KOHNAME         PIC X(16).
      *
           03  WRK-Z4              PIC ZZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z7              PIC ZZZ,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
      *
           03  WRK-RECEERR         PIC X(200).
      *
      *    文字変換
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
      *
           03  WRK-MCP-TABLE       PIC X(100).
           03  WRK-MCP-PATH        PIC X(100).
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
           03  WRK-KAISETUNAME       PIC X(80).
      *
      *    公費番号まとめエリア
       01  WRK-2007-AREA.
           03  WRK-2007-TBL        OCCURS  100.
               05  WRK-2007-MEISYO PIC X(44).
               05  WRK-2007-KOH-AREA.
                   07  WRK-2007-KOHNUM
                                   PIC X(03)   OCCURS  30.
           03  WRK-2007-MAX        PIC 9(03).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
           03  CNT-KOH             PIC 9(02).
      *
       01  SORT-AREA.
           03  SORT-TBL.
               05  SORT-KOH-TBL  OCCURS  20.
                   07  SORT-KOHNUM              PIC  X(03).
                   07  SORT-KOHNAME             PIC  X(10).
                   07  SORT-KOH-NYUGAI-TBL OCCURS 2.
                       09  SORT-K-KENSU         PIC  9(05).
                       09  SORT-K-NISSU         PIC  9(05).
                       09  SORT-K-TENSU         PIC  9(09).
                       09  SORT-K-ITBFTN        PIC  9(09).
                   07  SORT-K-SKENSU            PIC  9(05).
                   07  SORT-K-SNISSU            PIC  9(05).
                   07  SORT-K-SKINGK            PIC  9(09).
                   07  SORT-K-SFTNGK            PIC  9(09).
      *
      *     COPY    "ORCA-DBPATH".
            COPY    "COMMON-SPA".
      *
      *    固定項目
       01  WRK-CONS-AREA.
           03  WRK-CONS-SRYYM-201404   PIC X(06) VALUE "201404".
           03  WRK-CONS-SRYYM-201604   PIC X(06) VALUE "201604".
      *
      *在総診テーブル
       01  TBL-ZAITAKU-AREA.
           03  FILLER                  PIC X(09) VALUE "114007610".
           03  FILLER                  PIC X(09) VALUE "114007710".
           03  FILLER                  PIC X(09) VALUE "114007510".
           03  FILLER                  PIC X(09) VALUE "114012210".
           03  FILLER                  PIC X(09) VALUE "114012310".
           03  FILLER                  PIC X(09) VALUE "114012410".
           03  FILLER                  PIC X(09) VALUE "114018710".
           03  FILLER                  PIC X(09) VALUE "114018810".
           03  FILLER                  PIC X(09) VALUE "114018910".
           03  FILLER                  PIC X(09) VALUE "114019010".
           03  FILLER                  PIC X(09) VALUE "114019510".
           03  FILLER                  PIC X(09) VALUE "114019610".
           03  FILLER                  PIC X(09) VALUE "114019710".
           03  FILLER                  PIC X(09) VALUE "114019810".
           03  FILLER                  PIC X(09) VALUE "114013010".
           03  FILLER                  PIC X(09) VALUE "114013110".
           03  FILLER                  PIC X(09) VALUE "114013210".
           03  FILLER                  PIC X(09) VALUE "114013310".
           03  FILLER                  PIC X(09) VALUE "114019110".
           03  FILLER                  PIC X(09) VALUE "114019210".
           03  FILLER                  PIC X(09) VALUE "114019310".
           03  FILLER                  PIC X(09) VALUE "114019410".
      *
       01  TBL-ZAITAKU-R  REDEFINES   TBL-ZAITAKU-AREA.
           03  TBL-ZAITAKU             PIC X(09) OCCURS  22.
      *
       01  TBL-ZAITAKU-MAX             PIC 9(02) VALUE   22.
      *
      *在総診テーブル（２０１４年４月診療分〜）
       01  TBL-ZAITAKU2-AREA.
           03  FILLER                  PIC X(09) VALUE "114007610".
           03  FILLER                  PIC X(09) VALUE "114007710".
           03  FILLER                  PIC X(09) VALUE "114019510".
           03  FILLER                  PIC X(09) VALUE "114019610".
           03  FILLER                  PIC X(09) VALUE "114019710".
           03  FILLER                  PIC X(09) VALUE "114019810".
           03  FILLER                  PIC X(09) VALUE "114023110".
           03  FILLER                  PIC X(09) VALUE "114023210".
           03  FILLER                  PIC X(09) VALUE "114023310".
           03  FILLER                  PIC X(09) VALUE "114023410".
           03  FILLER                  PIC X(09) VALUE "114023510".
           03  FILLER                  PIC X(09) VALUE "114023610".
           03  FILLER                  PIC X(09) VALUE "114023710".
           03  FILLER                  PIC X(09) VALUE "114023810".
           03  FILLER                  PIC X(09) VALUE "114023910".
           03  FILLER                  PIC X(09) VALUE "114024010".
           03  FILLER                  PIC X(09) VALUE "114024110".
           03  FILLER                  PIC X(09) VALUE "114024210".
           03  FILLER                  PIC X(09) VALUE "114024310".
           03  FILLER                  PIC X(09) VALUE "114024410".
           03  FILLER                  PIC X(09) VALUE "114024510".
           03  FILLER                  PIC X(09) VALUE "114024610".
           03  FILLER                  PIC X(09) VALUE "114024910".
           03  FILLER                  PIC X(09) VALUE "114025010".
           03  FILLER                  PIC X(09) VALUE "114025110".
           03  FILLER                  PIC X(09) VALUE "114025210".
           03  FILLER                  PIC X(09) VALUE "114025310".
           03  FILLER                  PIC X(09) VALUE "114025410".
           03  FILLER                  PIC X(09) VALUE "114025510".
           03  FILLER                  PIC X(09) VALUE "114025610".
           03  FILLER                  PIC X(09) VALUE "114025710".
           03  FILLER                  PIC X(09) VALUE "114025810".
           03  FILLER                  PIC X(09) VALUE "114025910".
           03  FILLER                  PIC X(09) VALUE "114026010".
           03  FILLER                  PIC X(09) VALUE "114026110".
           03  FILLER                  PIC X(09) VALUE "114026210".
           03  FILLER                  PIC X(09) VALUE "114026310".
           03  FILLER                  PIC X(09) VALUE "114026410".
      *
       01  TBL-ZAITAKU2-R  REDEFINES   TBL-ZAITAKU2-AREA.
           03  TBL-ZAITAKU2            PIC X(09) OCCURS  38.
      *
       01  TBL-ZAITAKU2-MAX            PIC 9(02) VALUE   38.
      *
      *    在総診テーブル（２０１６年４月診療分〜）
       01  TBL-ZAITAKU3-AREA.
      *    在がん医総（在支診等）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114007610".
      *    在がん医総（在支診等）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114007710".
      *    在医総・在医総管（ＤＵＭＭＹ）
           03  FILLER          PIC X(09)   VALUE   "099140003".
      *
      *    在がん医総（機能強化した在支診等）（病床あり）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114019510".
      *    在がん医総（機能強化した在支診等）（病床あり）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114019610".
      *    在がん医総（機能強化した在支診等）（病床なし）（処方せんあり）
           03  FILLER          PIC X(09)   VALUE   "114019710".
      *    在がん医総（機能強化した在支診等）（病床なし）（処方せんなし）
           03  FILLER          PIC X(09)   VALUE   "114019810".
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
       01  TBL-ZAITAKU3-R      REDEFINES   TBL-ZAITAKU3-AREA.
            03  TBL-ZAITAKU3   PIC X(09)   OCCURS  79.
      *
       01  TBL-ZAITAKU3-MAX    PIC 9(02)   VALUE   79.
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
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
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
           ACCEPT     SYS-TIME          FROM    TIME
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
           MOVE    "SOKATU3000"    TO  JOB-PGID
           MOVE    "国保請求書"
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "SKT3000"         TO  SKT3000TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT3000TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT3000TP-HOSPNUM
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  SKT3000TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT3000TP-FULLNAME
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
               MOVE    ZERO                TO  IDZ1
               PERFORM VARYING IDX2 FROM    1   BY  1
                       UNTIL   IDX2 >       10
                       OR      SYS-2007-KOHNUM (IDX2)   =   SPACE
                   ADD     1               TO  IDZ1
                   MOVE    SYS-2007-KOHNUM (IDX2)
                                           TO WRK-2007-KOHNUM(IDX1 IDZ1)
               END-PERFORM
               PERFORM VARYING IDX2 FROM    1   BY  1
                       UNTIL   IDX2 >       20
                       OR      SYS-2007-KOHNUM1(IDX2)   =   SPACE
                   ADD     1               TO  IDZ1
                   MOVE    SYS-2007-KOHNUM1(IDX2)
                                          TO  WRK-2007-KOHNUM(IDX1 IDZ1)
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
           EVALUATE    WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
               WHEN   "2"               ALSO  "1"
                   MOVE    "key37"        TO  WRK-MCP-PATH
                   MOVE    "key65"        TO  WRK-PATH2
               WHEN   "1"               ALSO  "1"
                   MOVE    "key38"        TO  WRK-MCP-PATH
                   MOVE    "key66"        TO  WRK-PATH2
               WHEN   "2"               ALSO  "2"
                   MOVE    "key39"        TO  WRK-MCP-PATH
                   MOVE    "key65"        TO  WRK-PATH2
               WHEN   "1"               ALSO  "2"
                   MOVE    "key40"        TO  WRK-MCP-PATH
                   MOVE    "key66"        TO  WRK-PATH2
           END-EVALUATE
           MOVE    "tbl_seikyu"           TO  WRK-MCP-TABLE
      *
           OPEN    OUTPUT  UPD-FILE
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
           MOVE    "SOKATU3000"              TO  WRK-PRGID
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
           OPEN     OUTPUT  SKT3000TMP-FILE
           CLOSE            SKT3000TMP-FILE
           OPEN     I-O     SKT3000TMP-FILE
      *
           MOVE     ZERO    TO      FLG-END
           MOVE     ZERO    TO      FLG-UPD
      *
           PERFORM  2001-KOKUHO-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
      *
           CLOSE   UPD-FILE
           CLOSE   SKT3000TMP-FILE
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
           DISPLAY "*** SOKATU3000 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU3000 END ***"
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *
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
           IF  RECE10-HKNNUM   =  "039" OR "040"
               PERFORM 900-SEIKYU-READ-SEC
           END-IF
      *TMP-FILE キーセット
      *
           INITIALIZE       SKT3000TMP-REC
      *
      *    保険者番号
           MOVE  RECE10-HKNJANUM-S       TO  SKT3000TMP-HKNJANUM
      *
      *    出力順設定
      *    (優先保険者→県内市町村→県内国保組合→全国国保組合→県外)
           IF    WRK-YUSEN  =  SKT3000TMP-HKNJANUM
             MOVE          1   TO    SKT3000TMP-SYUTURYOKU
           ELSE
             IF    RECE10-PREFKBN  =  "1"
               IF  SKT3000TMP-HKNJANUM(3:1)  =  "3"
                 MOVE  3        TO    SKT3000TMP-SYUTURYOKU
               ELSE
                 MOVE  2        TO    SKT3000TMP-SYUTURYOKU
               END-IF
             ELSE
               MOVE    5        TO    SKT3000TMP-SYUTURYOKU
             END-IF
           END-IF
      *
      * 和歌山カスタマイズ追加
           MOVE    RECE10-NYUGAIKBN      TO  SKT3000TMP-NYUGAIKBN
      * 診療年月が10月以前の時
             IF  RECE10-SRYYM            <   200610
                 MOVE    1               TO  SKT3000TMP-KBN
             ELSE
                 IF  RECE10-SRYYM        <   200804
                   MOVE    2             TO  SKT3000TMP-KBN
                 ELSE
                   MOVE    3             TO  SKT3000TMP-KBN
                 END-IF      
             END-IF
      *
      *    在宅・在医総
           MOVE    RECE10-ZAITAKU        TO  SKT3000TMP-ZAITAKU
      *
      *    在総診・在医総か
           IF  RECE10-ZAITAKU  =  SPACE
             PERFORM  960-SRYACT-SET-SEC
           END-IF
      *
      * 和歌山カスタマイズ追加
           IF  RECE10-ETC-KOHNUM(1) = "972" OR "974" OR
               RECE10-ETC-KOHNUM(2) = "972" OR "974" OR
               RECE10-ETC-KOHNUM(3) = "972" OR "974" OR
               RECE10-ETC-KOHNUM(4) = "972" OR "974"
               IF  RECE10-ZAITAKU   =  SPACE
                   MOVE  1          TO  SKT3000TMP-MARUCHO
               END-IF
           END-IF

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
           IF  RECE10-NYUGAIKBN  =  "1"
             MOVE  1      TO   IDX2
           ELSE
             MOVE  1      TO   IDX2
           END-IF
      *
           IF  RECE10-SRYYM            <   200804
      *      老人保健か
             IF  RECE10-RJNHKNNUM  NOT  =  SPACE
               MOVE    1    TO   SKT3000TMP-PAGE-FLG2
               IF  RECE10-KYURATE  =  9
                 MOVE  10   TO   IDX1
               ELSE
                 MOVE  11   TO   IDX1
               END-IF
             ELSE
               MOVE    1    TO   SKT3000TMP-PAGE-FLG1
      *      一般国保か
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
                   IF  RECE10-AGE  >= 3
                     MOVE    3     TO    IDX1
                   ELSE
                     MOVE    4     TO    IDX1
                   END-IF
                 END-IF
      *
               ELSE
                 IF  RECE10-AGE  >=  70
                   IF  RECE10-KYURATE  =  9
                     MOVE  6    TO  IDX1
                   ELSE
                     MOVE  7    TO  IDX1
                   END-IF
                 ELSE
                   IF  RECE10-HONKZKKBN  =  "1"
                     MOVE  5    TO   IDX1
                   ELSE
                     IF  RECE10-AGE  <  3
                       MOVE  9  TO  IDX1
                     ELSE
                       MOVE  8  TO  IDX1
                     END-IF
                   END-IF
                 END-IF
               END-IF
             END-IF
           ELSE
      *      老人保健か
             IF  RECE10-RJNHKNNUM  NOT  =  SPACE
               MOVE    1    TO   SKT3000TMP-PAGE-FLG2
               IF  RECE10-KYURATE  =  9
                 MOVE  10   TO   IDX1
               ELSE
                 MOVE  11   TO   IDX1
               END-IF
             ELSE
               MOVE    1    TO   SKT3000TMP-PAGE-FLG1
      *      一般国保か
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
                   IF  RECE10-FLG (1)  NOT = 1
                     MOVE    3     TO    IDX1
                   ELSE
                     MOVE    4     TO    IDX1
                   END-IF
                 END-IF
               ELSE
                 IF  RECE10-HONKZKKBN  =  "1"
                   MOVE  5    TO   IDX1
                 ELSE
                   IF  RECE10-FLG (1)  = 1
                     MOVE  9  TO  IDX1
                   ELSE
                     MOVE  8  TO  IDX1
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF
      *
           PERFORM  2101-SYUKEI-SEC
      *
      **    公費負担分
           IF  RECE10-KOHNUM (1)   NOT =   SPACE
             IF  RECE10-RJNHKNNUM  NOT =   SPACE
               PERFORM 220-RKOH-SYUKEI-SEC
             ELSE
               PERFORM 220-KOH-SYUKEI-SEC
             END-IF
           END-IF
      *
      *    長期件数
           IF  RECE10-ETC-KOHNUM(1) = "972" OR "974" OR
               RECE10-ETC-KOHNUM(2) = "972" OR "974" OR
               RECE10-ETC-KOHNUM(3) = "972" OR "974" OR
               RECE10-ETC-KOHNUM(4) = "972" OR "974"
             EVALUATE  IDX1
             WHEN 1 THRU  4
               ADD     1          TO   SKT3000TMP-TYOGEN(1)
             WHEN 5 THRU  9
               ADD     1          TO   SKT3000TMP-TYOGEN(2)
             WHEN OTHER
               ADD     1          TO   SKT3000TMP-TYOGEN(3)
             END-EVALUATE
           END-IF
      *
      *    原爆件数
           IF  RECE10-ETC-KOHNUM(1) = "019" OR
               RECE10-ETC-KOHNUM(2) = "019" OR
               RECE10-ETC-KOHNUM(3) = "019" OR
               RECE10-ETC-KOHNUM(4) = "019"
               ADD     1          TO   SKT3000TMP-TYOGEN(4)
           END-IF
      *
           IF  FLG-SKT3000TMP  =  1
               WRITE    SKT3000TMP-REC
           ELSE
               REWRITE  SKT3000TMP-REC
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
           ADD   1                  TO  SKT3000TMP-KENSU(IDX1 IDX2)
      *    療養の給付:診療実日数
           ADD  RECE10-JNISSU(1)    TO  SKT3000TMP-NISSU(IDX1 IDX2)
      *    療養の給付：点数
           ADD  RECE10-TOTALTEN(1)  TO  SKT3000TMP-TENSU(IDX1 IDX2)
      *    療養の給付：一部負担金
           ADD  RECE10-FTNMONEY(1)  TO  SKT3000TMP-ITBFTN(IDX1 IDX2)
      *
           IF  RECE10-SHOKUJINISSU(1)  >  0
      *    食事療養:件数
             ADD   1  TO  SKT3000TMP-SKENSU(IDX1)
           END-IF
      *    食事療養:日数
           ADD  RECE10-SHOKUJINISSU(1)   TO  SKT3000TMP-SNISSU(IDX1)
      *    食事療養：金額
           ADD  RECE10-SHOKUJIRYOYOHI(1) TO  SKT3000TMP-SKINGK(IDX1)
      *    食事療養：標準負担額
           ADD  RECE10-SHOKUJIFTN(1)     TO  SKT3000TMP-SFTNGK(IDX1)
      *
           MOVE    1               TO  FLG-UPD
      *
      *    一般・退職の場合は合計加算
           IF  RECE10-RJNHKNNUM  =  SPACE
             IF  RECE10-HKNNUM   =  "060" OR "068"
               ADD 1                         TO  SKT3000TMP-GKENSU(1)
               ADD RECE10-FTNMONEY(1)        TO  SKT3000TMP-GKINGK(1)
               ADD RECE10-SHOKUJIRYOYOHI(1)  TO  SKT3000TMP-GKINGK(1)
               ADD RECE10-SHOKUJIFTN(1)      TO  SKT3000TMP-GKINGK(1)
             ELSE
               ADD 1                         TO  SKT3000TMP-GKENSU(2)
               ADD RECE10-FTNMONEY(1)        TO  SKT3000TMP-GKINGK(2)
               ADD RECE10-SHOKUJIRYOYOHI(1)  TO  SKT3000TMP-GKINGK(2)
               ADD RECE10-SHOKUJIFTN(1)      TO  SKT3000TMP-GKINGK(2)
             END-IF
           END-IF
           .
       2101-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担分集計処理(一般・退職分)
      *****************************************************************
       220-KOH-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDX3     FROM    1   BY  1
                   UNTIL   IDX3     >       4
                   OR  RECE10-KOHNUM (IDX3) =   SPACE
      *
      *    ０５１と０９１は同じ場所に集計する
             IF    RECE10-KOHNUM(IDX3)  =  "091"
               MOVE    "051"    TO    RECE10-KOHNUM(IDX3)
             END-IF
      *
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
      *
             PERFORM  VARYING  IDX4  FROM  1  BY  1
                      UNTIL    IDX4  >     20
                      OR       SKT3000TMP-KOHNUM (IDX4) = SPACE
               IF  RECE10-KOHNUM (IDX3)  =  SKT3000TMP-KOHNUM (IDX4)
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
             MOVE  RECE10-KOHNUM(IDX3)  TO  SKT3000TMP-KOHNUM(CNT-KOH)
      *    公費名称
             MOVE  WRK-KOHNAME          TO  SKT3000TMP-KOHNAME(CNT-KOH)
      *    療養の給付：件数
             ADD   1  TO  SKT3000TMP-K-KENSU (CNT-KOH IDX2)
      *
      *    療養の給付:診療実日数
             ADD  RECE10-JNISSU(IDX3 + 1)    TO
                               SKT3000TMP-K-NISSU(CNT-KOH IDX2)
      *    療養の給付：点数
             ADD  RECE10-TOTALTEN(IDX3 + 1)  TO
                               SKT3000TMP-K-TENSU(CNT-KOH IDX2)
      *    療養の給付：一部負担金
             ADD  RECE10-FTNMONEY(IDX3 + 1)  TO
                               SKT3000TMP-K-ITBFTN(CNT-KOH IDX2)
      *
             IF  RECE10-SHOKUJINISSU(IDX3 + 1)  >  0
      *    食事療養:件数
               ADD   1  TO  SKT3000TMP-K-SKENSU(CNT-KOH)
             END-IF
      *    食事療養:日数
             ADD  RECE10-SHOKUJINISSU(IDX3 + 1)    TO
                               SKT3000TMP-K-SNISSU(CNT-KOH)
      *    食事療養：金額
             ADD  RECE10-SHOKUJIRYOYOHI(IDX3 + 1)  TO
                               SKT3000TMP-K-SKINGK(CNT-KOH)
      *    食事療養：標準負担額
             ADD  RECE10-SHOKUJIFTN(IDX3 + 1)      TO
                               SKT3000TMP-K-SFTNGK(CNT-KOH)
      *   合計加算
             IF  RECE10-HKNNUM   =  "060" OR "068"
               ADD RECE10-FTNMONEY(IDX3 + 1) TO  SKT3000TMP-GKINGK(1)
             ELSE
               ADD RECE10-FTNMONEY(IDX3 + 1) TO  SKT3000TMP-GKINGK(2)
             END-IF
      *
           END-PERFORM
      *
      *
           .
       220-KOH-SYUKEI-EXT.
           EXIT.
057400*
      *****************************************************************
      *    公費負担分集計処理(老人分)
      *****************************************************************
       220-RKOH-SYUKEI-SEC        SECTION.
      *
           PERFORM VARYING IDX3     FROM    1   BY  1
                   UNTIL   IDX3     >       4
                   OR  RECE10-KOHNUM (IDX3) =   SPACE
      *
      *    ０５１と０９１は同じ場所に集計する
             IF    RECE10-KOHNUM(IDX3)  =  "091"
               MOVE    "051"    TO    RECE10-KOHNUM(IDX3)
             END-IF
      *
             PERFORM  VARYING  IDX4  FROM  1  BY  1
                      UNTIL    IDX4  >     20
                      OR       SKT3000TMP-RKOHNUM (IDX4) = SPACE
               IF  RECE10-KOHNUM (IDX3)  =  SKT3000TMP-RKOHNUM (IDX4)
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
             MOVE  RECE10-KOHNUM(IDX3)  TO  SKT3000TMP-RKOHNUM(CNT-KOH)
      *    療養の給付：件数
             ADD   1  TO  SKT3000TMP-K-RKENSU (CNT-KOH IDX2)
      *
      *    療養の給付:診療実日数
             ADD  RECE10-JNISSU(IDX3 + 1)    TO
                               SKT3000TMP-K-RNISSU(CNT-KOH IDX2)
      *    療養の給付：点数
             ADD  RECE10-TOTALTEN(IDX3 + 1)  TO
                               SKT3000TMP-K-RTENSU(CNT-KOH IDX2)
      *    療養の給付：一部負担金
             ADD  RECE10-FTNMONEY(IDX3 + 1)  TO
                               SKT3000TMP-K-RITBFTN(CNT-KOH IDX2)
      *
             IF  RECE10-SHOKUJINISSU(IDX3 + 1)  >  0
      *    食事療養:件数
               ADD   1  TO  SKT3000TMP-K-RSKENSU(CNT-KOH)
             END-IF
      *    食事療養:日数
             ADD  RECE10-SHOKUJINISSU(IDX3 + 1)    TO
                               SKT3000TMP-K-RSNISSU(CNT-KOH)
      *    食事療養：金額
             ADD  RECE10-SHOKUJIRYOYOHI(IDX3 + 1)  TO
                               SKT3000TMP-K-RSKINGK(CNT-KOH)
      *    食事療養：標準負担額
             ADD  RECE10-SHOKUJIFTN(IDX3 + 1)      TO
                               SKT3000TMP-K-RSFTNGK(CNT-KOH)
      *
           END-PERFORM
      *
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
           INITIALIZE   SKT3000TMP-REC
           OPEN     INPUT       SKT3000TMP-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =   1
      *
             PERFORM  KOH-SORT-SEC
      *    該当データが無い場合は印刷しない
             IF    SKT3000TMP-PAGE-FLG1   =   1
               MOVE  ZERO  TO  FLG-PASS
               MOVE  1     TO  WRK-CNT1
               PERFORM  UNTIL  FLG-PASS   =   1  AND
                        SKT3000TMP-KOHNUM(WRK-CNT1) = SPACE
      *    １枚目(一般＋退職)
      *    帳票編集<見出し>処理
                 PERFORM 310-HEAD-HEN-SEC 
      *    帳票編集<明細>処理
                 PERFORM 320-BODY-HEN-SEC
      *    帳票印刷処理
                 PERFORM 330-PRINT-OUT-SEC
                 MOVE    1     TO    FLG-PASS
               END-PERFORM
             END-IF
      *
             IF    SKT3000TMP-PAGE-FLG2   =   1
               MOVE  ZERO  TO  FLG-PASS
               MOVE  1     TO  WRK-CNT2
               PERFORM  UNTIL  FLG-PASS   =   1  AND
                        SKT3000TMP-RKOHNUM(WRK-CNT2) = SPACE
      *    １枚目(一般＋退職)
      *    帳票編集<見出し>処理
                 PERFORM 311-HEAD-HEN-SEC 
      *    帳票編集<明細>処理
                 PERFORM 321-BODY-HEN-SEC
      *    帳票印刷処理
                 PERFORM 331-PRINT-OUT-SEC
                 MOVE    1     TO    FLG-PASS
               END-PERFORM
             END-IF
      *
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE    SKT3000TMP-FILE
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
           INITIALIZE                        SKT3000
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT3000-PRTYM
      *    請求年月日
           MOVE    WRK-SEIYMDWH          TO  SKT3000-SEIYMD
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT3000-ADRS
           INSPECT SKT3000-ADRS          REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT3000-HOSPNAME
           INSPECT SKT3000-HOSPNAME      REPLACING ALL "  " BY "　"
      *    電話番号
           MOVE    WRK-HOSPTEL           TO  SKT3000-TEL
      *    開設者名
           MOVE    WRK-KAISETUNAME       TO  SKT3000-KAISETUNAME
      *    保険者番号
           INITIALIZE                        WRK-MAE-INPUT
           MOVE    SKT3000TMP-HKNJANUM   TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT         TO  SKT3000-HKNJANUM
      *    保険者名
           MOVE    SKT3000TMP-HKNJANUM   TO  WRK-HKNJANUM
           PERFORM 920-HKNJAINF-SET-SEC
           IF      HKNJA-HKNJANAME    NOT  =  SPACE
             STRING  HKNJA-HKNJANAME    DELIMITED BY SPACE
                     "  殿"             DELIMITED BY SIZE
                     INTO               SKT3000-HKNJANAME
             END-STRING
           ELSE
             MOVE  "殿"           TO  SKT3000-HKNJANAME(23:2)
           END-IF
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD       TO  SKT3000-HOSPCD
      *    在総診・在医総
           IF    SKT3000TMP-ZAITAKU  =  "1"
             MOVE    "在"                TO  SKT3000-ZAIMOJI
             MOVE    "◯"                TO  SKT3000-ZAIMARU
           END-IF
      * 和歌山カスタマイズ追加
           IF    SKT3000TMP-NYUGAIKBN    =  "1"
             MOVE    "○"                TO  SKT3000-NYUIN
           ELSE
             MOVE    "○"                TO  SKT3000-GAIRAI
           END-IF
           IF    SKT3000TMP-MARUCHO      =   1
             MOVE    "長"                TO  SKT3000-ZAIMOJI
             MOVE    "◯"                TO  SKT3000-ZAIMARU
           END-IF
      *
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT3000-TOKUBETU
           END-IF
      *
      *    ページ番号の追加
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT3000-NO
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
           IF  WRK-CNT1      =     1
             PERFORM  VARYING  IDX1  FROM    1   BY  1
                      UNTIL    IDX1  >  9
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     2
      *    療養の給付：件数
                 MOVE  SKT3000TMP-KENSU(IDX1 IDX2)   TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT3000-KENSU(IDX1 IDX2)
      *    療養の給付：日数
                 MOVE  SKT3000TMP-NISSU(IDX1 IDX2)   TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT3000-NISSU(IDX1 IDX2)
      *    療養の給付：点数
                 MOVE  SKT3000TMP-TENSU(IDX1 IDX2)   TO  WRK-Z9
                 MOVE  WRK-Z9   TO  SKT3000-TENSU(IDX1 IDX2)
      *    療養の給付：一部負担金
                 MOVE  SKT3000TMP-ITBFTN (IDX1 IDX2) TO  WRK-Z9
                 MOVE  WRK-Z9   TO  SKT3000-ITBFTN(IDX1 IDX2)
               END-PERFORM
      *    食事療養：件数
               MOVE  SKT3000TMP-SKENSU(IDX1)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3000-SKENSU(IDX1)
      *    食事療養：日数
               MOVE  SKT3000TMP-SNISSU(IDX1)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3000-SNISSU(IDX1)
      *    食事療養：金額
               MOVE  SKT3000TMP-SKINGK(IDX1)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3000-SKINGK(IDX1)
      *    食事療養：標準負担額
               MOVE  SKT3000TMP-SFTNGK(IDX1)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3000-SFTNGK(IDX1)
             END-PERFORM
      *
      *    合計編集
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                      UNTIL    IDX1  >     2
      *
      *    長期
               MOVE  SKT3000TMP-TYOGEN(IDX1)  TO  WRK-Z7
               MOVE  WRK-Z7    TO  SKT3000-TYOKI(IDX1)
             END-PERFORM
           END-IF
      *
      *    公費の印字
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     8
                    OR  WRK-CNT1   >     20
                    OR  SKT3000TMP-KOHNUM(WRK-CNT1) = SPACE
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
      *    療養の給付：件数
               MOVE  SKT3000TMP-K-KENSU(WRK-CNT1 IDX2)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3000-K-KENSU(IDX3 IDX2)
      *    療養の給付：日数
               MOVE  SKT3000TMP-K-NISSU(WRK-CNT1 IDX2)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3000-K-NISSU(IDX3 IDX2)
      *    療養の給付：点数
               MOVE  SKT3000TMP-K-TENSU(WRK-CNT1 IDX2)   TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3000-K-TENSU(IDX3 IDX2)
      *    療養の給付：一部負担金
               MOVE  SKT3000TMP-K-ITBFTN(WRK-CNT1 IDX2)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3000-K-ITBFTN(IDX3 IDX2)
             END-PERFORM
      *    食事療養：件数
             MOVE  SKT3000TMP-K-SKENSU(WRK-CNT1)         TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3000-K-SKENSU(IDX3)
      *    食事療養：日数
             MOVE  SKT3000TMP-K-SNISSU(WRK-CNT1)         TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3000-K-SNISSU(IDX3)
      *    食事療養：金額
             MOVE  SKT3000TMP-K-SKINGK(WRK-CNT1)         TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3000-K-SKINGK(IDX3)
      *    食事療養：標準負担額
             MOVE  SKT3000TMP-K-SFTNGK(WRK-CNT1)         TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3000-K-SFTNGK(IDX3)
      *    公費名
             MOVE  SKT3000TMP-KOHNUM (WRK-CNT1)  TO  WRK-KOHNUM
             PERFORM  950-HKNNUM-SET-SEC
             MOVE  HKN-HBTNUM         TO  SKT3000-HBTNUM(IDX3)
             IF  SKT3000TMP-KOHNAME (WRK-CNT1)  NOT =  SPACE
               MOVE  SKT3000TMP-KOHNAME (WRK-CNT1)
                                       TO  SKT3000-KOHNAME (IDX3)
             ELSE
               MOVE  HKN-TANSEIDONAME  TO  SKT3000-KOHNAME (IDX3)
             END-IF
      *
             ADD  1  TO  WRK-CNT1
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
           IF  SKT3000TMP-KBN          =  "1"
               MOVE   "SKT3001.red"    TO  SPRT-PRTID
           ELSE
               IF  SKT3000TMP-KBN          =  "2"
                   MOVE   "SKT3000.red"    TO  SPRT-PRTID
               ELSE
                   MOVE   "SKT3002.red"    TO  SPRT-PRTID
               END-IF
           END-IF
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "国保請求書"      TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "国保請求書［返戻分］" 
                                       TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "国保請求書［特別療養費］"
                                       TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "国保請求書［返戻分］［特別療養費］"
                                       TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT3000             TO  SPRT-PRTDATA
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
      *    帳票編集<ヘッダー部>処理（老人・公費等）
      *****************************************************************
       311-HEAD-HEN-SEC          SECTION.
      *
           ADD     1                     TO  CNT-PAGE
      *
           INITIALIZE                        SKT3010
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT3010-PRTYM
      *    請求年月日
           MOVE    WRK-SEIYMDWH          TO  SKT3010-SEIYMD
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT3010-ADRS
           INSPECT SKT3010-ADRS          REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT3010-HOSPNAME
           INSPECT SKT3010-HOSPNAME      REPLACING ALL "  " BY "　"
      *    電話番号
           MOVE    WRK-HOSPTEL           TO  SKT3010-TEL
      *    開設者名
           MOVE    SYS-1001-KAISETUNAME  TO  SKT3010-KAISETUNAME
      *    保険者番号
           INITIALIZE                        WRK-MAE-INPUT
           MOVE    SKT3000TMP-HKNJANUM   TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT         TO  SKT3010-HKNJANUM
      *    保険者名
           MOVE    SKT3000TMP-HKNJANUM   TO  WRK-HKNJANUM
           PERFORM 920-HKNJAINF-SET-SEC
           IF      HKNJA-HKNJANAME    NOT  =  SPACE
             STRING  HKNJA-HKNJANAME    DELIMITED BY SPACE
                     "  殿"             DELIMITED BY SIZE
                     INTO               SKT3010-HKNJANAME
             END-STRING
           ELSE
             MOVE  "殿"           TO  SKT3010-HKNJANAME(23:2)
           END-IF
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD       TO  SKT3010-HOSPCD
      *    在総診・在医総
           IF    SKT3000TMP-ZAITAKU  =  "1"
             MOVE    "在"                TO  SKT3010-ZAIMOJI
             MOVE    "◯"                TO  SKT3010-ZAIMARU
           END-IF
      *
      * 和歌山カスタマイズ追加
           IF    SKT3000TMP-NYUGAIKBN    =  "1"
             MOVE    "○"                TO  SKT3010-NYUIN
           ELSE
             MOVE    "○"                TO  SKT3010-GAIRAI
           END-IF
           IF    SKT3000TMP-MARUCHO      =   1
             MOVE    "長"                TO  SKT3010-ZAIMOJI
             MOVE    "◯"                TO  SKT3010-ZAIMARU
           END-IF
      *    ページ番号の追加
           MOVE    CNT-PAGE              TO  WRK-Z4
           MOVE    WRK-Z4                TO  SKT3010-NO
      *
           .
       311-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理（公費等）
      *****************************************************************
       321-BODY-HEN-SEC      SECTION.
      *
      *    老人編集処理
           
           IF  WRK-CNT2      =     1
             MOVE     ZERO     TO    IDX4
             PERFORM  VARYING  IDX1  FROM   10   BY  1
                      UNTIL    IDX1  >  11
               ADD  1  TO  IDX4
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >     2
      *    療養の給付：件数
                 MOVE  SKT3000TMP-KENSU(IDX1 IDX2)  TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT3010-KENSU(IDX4 IDX2)
      *    療養の給付：日数
                 MOVE  SKT3000TMP-NISSU(IDX1 IDX2)  TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT3010-NISSU(IDX4 IDX2)
      *    療養の給付：点数
                 MOVE  SKT3000TMP-TENSU(IDX1 IDX2)  TO  WRK-Z9
                 MOVE  WRK-Z9   TO  SKT3010-TENSU(IDX4 IDX2)
      *    療養の給付：一部負担金
                 MOVE  SKT3000TMP-ITBFTN(IDX1 IDX2) TO  WRK-Z9
                 MOVE  WRK-Z9  TO  SKT3010-ITBFTN(IDX4 IDX2)
               END-PERFORM
      *    食事療養：件数
               MOVE  SKT3000TMP-SKENSU(IDX1)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3010-SKENSU(IDX4)
      *    食事療養：日数
               MOVE  SKT3000TMP-SNISSU(IDX1)  TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3010-SNISSU(IDX4)
      *    食事療養：金額
               MOVE  SKT3000TMP-SKINGK(IDX1)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3010-SKINGK(IDX4)
      *    食事療養：標準負担額
               MOVE  SKT3000TMP-SFTNGK(IDX1)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3010-SFTNGK(IDX4)
             END-PERFORM
      *
      *    長期
             PERFORM  VARYING  IDX1  FROM  3  BY  1
                      UNTIL    IDX1  >     4
               COMPUTE  IDX4  =  IDX1  -   2
               MOVE  SKT3000TMP-TYOGEN(IDX1)  TO  WRK-Z7
               MOVE  WRK-Z7    TO  SKT3010-TYOKI(IDX4)
             END-PERFORM
           END-IF
      *
      *    公費の印字
           PERFORM  VARYING  IDX3  FROM  1  BY  1
                    UNTIL    IDX3  >     12
                    OR  WRK-CNT2   >     20
                    OR  SKT3000TMP-RKOHNUM(WRK-CNT2) = SPACE
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
      *    療養の給付：件数
               MOVE  SKT3000TMP-K-RKENSU(WRK-CNT2 IDX2)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3010-K-KENSU(IDX3 IDX2)
      *    療養の給付：日数
               MOVE  SKT3000TMP-K-RNISSU(WRK-CNT2 IDX2)   TO  WRK-Z5
               MOVE  WRK-Z5   TO  SKT3010-K-NISSU(IDX3 IDX2)
      *    療養の給付：点数
               MOVE  SKT3000TMP-K-RTENSU(WRK-CNT2 IDX2)   TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3010-K-TENSU(IDX3 IDX2)
      *    療養の給付：一部負担金
               MOVE  SKT3000TMP-K-RITBFTN(WRK-CNT2 IDX2)  TO  WRK-Z9
               MOVE  WRK-Z9   TO  SKT3010-K-ITBFTN(IDX3 IDX2)
             END-PERFORM
      *    食事療養：件数
             MOVE  SKT3000TMP-K-RSKENSU(WRK-CNT2)         TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3010-K-SKENSU(IDX3)
      *    食事療養：日数
             MOVE  SKT3000TMP-K-RSNISSU(WRK-CNT2)         TO  WRK-Z5
             MOVE  WRK-Z5   TO  SKT3010-K-SNISSU(IDX3)
      *    食事療養：金額
             MOVE  SKT3000TMP-K-RSKINGK(WRK-CNT2)         TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3010-K-SKINGK(IDX3)
      *    食事療養：標準負担額
             MOVE  SKT3000TMP-K-RSFTNGK(WRK-CNT2)         TO  WRK-Z9
             MOVE  WRK-Z9   TO  SKT3010-K-SFTNGK(IDX3)
      *    公費名
             MOVE  SKT3000TMP-RKOHNUM (WRK-CNT2)  TO  WRK-KOHNUM
             PERFORM  950-HKNNUM-SET-SEC
             MOVE  HKN-HBTNUM         TO  SKT3010-HBTNUM(IDX3)
             MOVE  HKN-TANSEIDONAME   TO  SKT3010-KOHNAME(IDX3)      
      *
             ADD  1  TO  WRK-CNT2
           END-PERFORM
      *
           .
       321-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理（老人・公費等）
      *****************************************************************
       331-PRINT-OUT-SEC                SECTION.
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
           IF  SKT3000TMP-KBN          =  "1"
               MOVE   "SKT3011.red"    TO  SPRT-PRTID
           ELSE
               MOVE   "SKT3010.red"    TO  SPRT-PRTID
           END-IF
           MOVE    "国保請求書"        TO  SPRT-TITLE
           MOVE    SKT3010             TO  SPRT-PRTDATA
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
057400*
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
             IF  SKT3000TMP-KOHNUM(IDX1)  =  SPACE
               MOVE  20    TO    IDX1
             ELSE
               IF    WRK-KOHNUM               =   SPACE   OR
                     SKT3000TMP-KOHNUM(IDX1)  <   WRK-KOHNUM
                 MOVE     IDX1                TO  IDX2
                 MOVE     SKT3000TMP-KOHNUM(IDX1) TO  WRK-KOHNUM
               END-IF
               ADD   1     TO    IDX-MAX
             END-IF
           END-PERFORM
           IF     IDX2     >     ZERO
             MOVE    SKT3000TMP-KOH-TBL(IDX2) TO  SORT-KOH-TBL(1)
           END-IF
      *
      *
           IF      IDX-MAX    >     ZERO
             MOVE    2    TO    IDX1
             MOVE    1    TO    IDX2
             MOVE    1    TO    IDX3
             MOVE   "999"   TO    WRK-KOHNUM
             PERFORM    UNTIL    IDX1  >    IDX-MAX
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >    IDX-MAX
                 IF  SKT3000TMP-KOHNUM(IDX2)  >   SORT-KOHNUM(IDX1 - 1)
                   IF  SKT3000TMP-KOHNUM(IDX2)  <  WRK-KOHNUM
                     MOVE  SKT3000TMP-KOHNUM(IDX2)  TO WRK-KOHNUM
                     MOVE  IDX2          TO    IDX3
                   END-IF
                 END-IF
               END-PERFORM
               MOVE  SKT3000TMP-KOH-TBL(IDX3)  TO  SORT-KOH-TBL(IDX1)
               MOVE  "999"  TO    WRK-KOHNUM
               ADD   1    TO    IDX1
             END-PERFORM
           END-IF
      *
           INITIALIZE             SKT3000TMP-KOH-AREA
           MOVE  SORT-AREA  TO    SKT3000TMP-KOH-AREA
      *
      *
           INITIALIZE  SORT-AREA
      *    公費数を取得
           MOVE    ZERO    TO    IDX-MAX
                                 IDX2 
           MOVE    SPACE   TO    WRK-KOHNUM
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >  20
             IF  SKT3000TMP-RKOHNUM(IDX1)  =  SPACE
               MOVE  20    TO    IDX1
             ELSE
               IF    WRK-KOHNUM               =   SPACE   OR
                     SKT3000TMP-RKOHNUM(IDX1)  <   WRK-KOHNUM
                 MOVE     IDX1                TO  IDX2
                 MOVE     SKT3000TMP-RKOHNUM(IDX1) TO  WRK-KOHNUM
               END-IF
               ADD   1     TO    IDX-MAX
             END-IF
           END-PERFORM
           IF     IDX2     >     ZERO
             MOVE    SKT3000TMP-RKOH-TBL(IDX2) TO  SORT-KOH-TBL(1)
           END-IF
      *
           IF      IDX-MAX    >     ZERO
             MOVE    2    TO    IDX1
             MOVE    1    TO    IDX2
             MOVE    1    TO    IDX3
             MOVE   "999"   TO    WRK-KOHNUM
             PERFORM    UNTIL    IDX1  >    IDX-MAX
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >    IDX-MAX
                 IF  SKT3000TMP-RKOHNUM(IDX2) > SORT-KOHNUM(IDX1 - 1)
                   IF  SKT3000TMP-RKOHNUM(IDX2)  <  WRK-KOHNUM
                     MOVE  SKT3000TMP-RKOHNUM(IDX2)  TO WRK-KOHNUM
                     MOVE  IDX2          TO    IDX3
                   END-IF
                 END-IF
               END-PERFORM
               MOVE  SKT3000TMP-RKOH-TBL(IDX3)  TO  SORT-KOH-TBL(IDX1)
               MOVE  "999"  TO    WRK-KOHNUM
               ADD   1    TO    IDX1
             END-PERFORM
           END-IF
      *
           INITIALIZE             SKT3000TMP-RKOH-AREA
           MOVE  SORT-AREA  TO    SKT3000TMP-RKOH-AREA
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
                   MOVE    RECE10-REC             TO  MCPDATA-REC
                   MOVE    "DBUPDATE"             TO  MCP-FUNC
                   MOVE    "tbl_seikyu"           TO  MCP-TABLE
                   MOVE    "key"                  TO  MCP-PATHNAME           
                   CALL    "ORCDBMAIN"             USING
                                                MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
                   IF      MCP-RC          =   ZERO
                       PERFORM 900-UPD-FILE-READ-SEC
                   ELSE
                       DISPLAY "UPD ERR KEY="  RECE10-KEY 
                       MOVE  "*** SOKATU3000 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                   MOVE  "*** SOKATU3000 SEIKYU INVALID ***"
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
           MOVE  SYS-1001-HOSPNUM            TO  HKNJA-HOSPNUM
           MOVE  WRK-HKNJANUM                TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC                TO  MCPDATA-REC
           MOVE  "DBSELECT"                  TO  MCP-FUNC
           MOVE    "tbl_hknjainf"           TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME           
            CALL  "ORCDBMAIN"         USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
             PERFORM  925-HKNJAINF-READ-SEC
      *
      *    静岡市の新保険者番号でも静岡市と表示されるように対応
             IF  SKT3000TMP-HKNJANUM  =  "224006"  AND
                 HKNJA-HKNJANAME  =  SPACE
               MOVE  "静岡市"        TO  HKNJA-HKNJANAME
             END-IF
      *
             MOVE  HKNJA-HKNJANAME   TO  WRK-HKNJANAME
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME           
           CALL  "ORCDBMAIN"      USING
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
            MOVE    "tbl_hknjainf"        TO  MCP-TABLE
            MOVE    "key"                  TO  MCP-PATHNAME           
           MOVE  "DBFETCH"              TO  MCP-FUNC
           CALL   "ORCDBMAIN" USING
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
      *
           MOVE  SYS-1001-HOSPNUM            TO  HKN-HOSPNUM
           MOVE  WRK-KOHNUM                  TO  HKN-HKNNUM
           MOVE    LNK-PRTKANRI-SRYYM(1:6)   TO  HKN-TEKSTYMD(1:6)
           MOVE    "01"                      TO  HKN-TEKSTYMD(7:2)
           MOVE    HKN-TEKSTYMD              TO  HKN-TEKEDYMD
      *
           MOVE  HKNNUM-REC                  TO  MCPDATA-REC
           MOVE  "DBSELECT"                  TO  MCP-FUNC
           MOVE    "tbl_hknnum"              TO  MCP-TABLE
           MOVE    "key5"                    TO  MCP-PATHNAME           
           CALL   "ORCDBMAIN"         USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_hknnum"             TO  MCP-TABLE
             MOVE    "key5"                   TO  MCP-PATHNAME           
             PERFORM  955-HKNNUM-READ-SEC
           ELSE
             DISPLAY  "HKNNUM SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"         TO  MCP-FUNC
           MOVE    "tbl_hknnum"          TO  MCP-TABLE
           MOVE    "key5"                TO  MCP-PATHNAME           
           CALL  "ORCDBMAIN"      USING
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
           MOVE  "DBFETCH"    TO  MCP-FUNC
           CALL   "ORCDBMAIN"  USING
                              MCPAREA
                              MCPDATA-REC
                              SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNNUM-REC
             MOVE  ZERO         TO  FLG-HKNNUM
             IF    HKN-HKNNUM  =  "051"
               INITIALIZE                HKN-TANSEIDONAME
               MOVE    "特定疾患"   TO   HKN-TANSEIDONAME
             END-IF
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
               IF  RECE10-SRYYM  >=  WRK-CONS-SRYYM-201604
                 PERFORM  VARYING  IDX5  FROM  1  BY  1
                          UNTIL   (IDX5  >  TBL-ZAITAKU3-MAX)
                          OR      (SKT3000TMP-ZAITAKU  =  "1")
                   IF  SRY-SRYCD(IDX4)  =  TBL-ZAITAKU3(IDX5)
                       MOVE    "1"            TO  SKT3000TMP-ZAITAKU
                   END-IF
                 END-PERFORM
               ELSE
                 IF  RECE10-SRYYM  >=  WRK-CONS-SRYYM-201404
                   PERFORM  VARYING  IDX5  FROM  1  BY  1
                            UNTIL   (IDX5  >  TBL-ZAITAKU2-MAX)
                            OR      (SKT3000TMP-ZAITAKU  =  "1")
                     IF  SRY-SRYCD(IDX4)  =  TBL-ZAITAKU2(IDX5)
                         MOVE    "1"            TO  SKT3000TMP-ZAITAKU
                     END-IF
                   END-PERFORM
                 ELSE
                   PERFORM  VARYING  IDX5  FROM  1  BY  1
                            UNTIL   (IDX5  >  TBL-ZAITAKU-MAX)
                            OR      (SKT3000TMP-ZAITAKU  =  "1")
                     IF  SRY-SRYCD(IDX4)  =  TBL-ZAITAKU(IDX5)
                         MOVE    "1"            TO  SKT3000TMP-ZAITAKU
                     END-IF
                   END-PERFORM
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
      *     MOVE   SYS-1001-HOSPID       TO    RECE10-HOSPID.
           MOVE   SPA-HOSPNUM      TO    RECE10-HOSPNUM
           MOVE   LNK-PRTKANRI-SRYYM    TO    RECE10-SKYYM.
      *
           IF    WRK-PARA-HKNJANUM  NOT = ZERO
               MOVE  WRK-PARA-HKNJANUM     TO  RECE10-XXSTHKNJANUM
                                               RECE10-XXEDHKNJANUM
           ELSE
               MOVE    SPACE               TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"          TO  RECE10-XXEDHKNJANUM
           END-IF
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  000000               TO  RECE10-SRYYM
             MOVE  999999               TO  RECE10-XXSRYYM
           END-IF
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE   WRK-MCP-TABLE        TO  MCP-TABLE
           MOVE   WRK-MCP-PATH         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
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
           MOVE   WRK-MCP-TABLE        TO  MCP-TABLE
           MOVE   WRK-MCP-PATH         TO  MCP-PATHNAME
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"          USING
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
               MOVE    SPA-HOSPNUM      TO  PTINF-HOSPNUM
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
               MOVE    1                   TO  FLG-END
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
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_ptinf"             TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME           
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
               MOVE    "tbl_ptinf"             TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME           
               CALL    "ORCDBMAIN"          USING
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
           MOVE    "tbl_ptinf"            TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME           
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
           READ    SKT3000TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT3000TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT3000TMP
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
           READ  SKT3000TMP-FILE   NEXT
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
           MOVE    "DBSELECT"                   TO  MCP-FUNC
           MOVE    "tbl_syskanri"            TO  MCP-TABLE
           MOVE    "key10"                   TO  MCP-PATHNAME           
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                    TO  MCP-FUNC
               MOVE    "tbl_syskanri"            TO  MCP-TABLE
               MOVE    "key10"                   TO  MCP-PATHNAME           
               CALL    "ORCDBMAIN"         USING
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
           MOVE    "tbl_syskanri"            TO  MCP-TABLE
           MOVE    "key10"                   TO  MCP-PATHNAME           
            CALL    "ORCDBMAIN"         USING
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
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME           
            CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
               MOVE    "tbl_seikyu"            TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME           
               CALL    "ORCDBMAIN"         USING
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
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME           
           CALL    "ORCDBMAIN"         USING
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
                                               SYS-1900-EDYUKYMD(1:6)
           ELSE
               MOVE    WRK-PARA-EDSRYYM    TO  SYS-1900-STYUKYMD(1:6)
                                               SYS-1900-EDYUKYMD(1:6)
           END-IF
           MOVE    "01"                    TO  SYS-1900-STYUKYMD(7:2)
                                               SYS-1900-EDYUKYMD(7:2)
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
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA.
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
          CALL     "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
           .                             
       900-DBDISCONNECT-EXT.
           EXIT.
