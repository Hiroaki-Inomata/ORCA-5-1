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
       PROGRAM-ID.             SOKATU4100.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国保・請求書集計処理(国保総括表)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/04/25     山口         ORCAのバージョンアップ(2.5.0)に伴う再作成
      *                            診療科名の出力方法変更
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    有村       05/05/06    医療機関情報取得条件修正
      *  01.00.02    内田       07/09/30    日レセver4.0対応
      *                                     システム管理マスタ「1901」の
      *                                     医療機関情報を参照
      *                                     返戻対応
      *  01.00.03    内田       07/12/05    政令指定都市は市でまとめる
      *  01.00.04    内田K      08/04/26    ４月改正対応
      *                                     SPA-AREA初期化
      *  01.00.05    内田       08/05/11    国組３〜６歳は帳票を分けない
      *  01.00.06    内田       08/06/18    国保がない場合でも印字する
      *  01.00.07    吉川       08/12/24    特別療養費国保対応
      *  01.00.08    吉川       09/03/19    札幌市と仙台市の
      *                                     代表保険者番号変更
      *  01.00.09    吉川       09/05/15    帳票様式の変更に伴う修正
      *  01.00.10    吉川       10/05/17    オンライン返戻対応
      *  01.00.11    吉川       11/01/12    open-cobol1.0対応
      *                                     特別療養費分を出力時
      *                                     印刷画面の帳票タイトル名修正
      *  01.00.12    吉川       11/04/18    後期高齢の記載順を県外→県内に修正
      *  01.00.13    吉川       13/02/07    プラグイン対応
      *                                     医療機関情報編集対応
      *  01.00.14    吉川       13/06/17    プログラムオプション対応
      *  01.00.15    吉川       14/10/27    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.16    門間       16/02/15    医療機関編集情報への開設者名追加対応
      *  01.00.17    門間       16/10/04    返戻時、再印刷画面のタイトル修正
      *  01.00.18    萩野       17/04/14    点数欄の印字桁数増加
      *                                     医療機関コード変更時、
      *                                     医療機関コード別の作成処理対応
      *  01.00.19    萩野       17/10/13    SITEKBN変更（動作に変更なし）
      *  01.00.20    萩野       19/02/07    修正履歴コメント削除
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    請求書一時ファイル  
           SELECT  SKT4100TMP-FILE ASSIGN    SKT4100TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT4100TMP-KEY
                                   FILE    STATUS  IS  STS-SKT4100.
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
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    請求書一時ファイル  
       FD  SKT4100TMP-FILE.
           COPY    "CPSKT4100TMP.INC".
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
                                   BY         //SKT4100TP//.
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
           COPY    "SKT4100.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT4100         PIC X(02).
           03  STS-RECE10          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-YUSEN           PIC X(02).
           03  STS-SRYKA           PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT4100TMP      PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-KOUKI           PIC 9(01).
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
           03  IDX                 PIC 9(05).
           03  IDY                 PIC 9(04).
      *
      *##  パラメタ領域（地方公費仕様）
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
           03  WRK-SRYKANAME       PIC X(20) OCCURS 3.
      *  
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-SUM  OCCURS  2.
               05  WRK-GNYUGAI-TBL  OCCURS  2.
                   07  WRK-GKENSU  PIC 9(05).
                   07  WRK-GNISSU  PIC 9(05).
                   07  WRK-GTENSU  PIC 9(09).
           03  WRK-HEIKIN               PIC 9(07)V9(1).
           03  WRK-HEIKIN-Z             PIC ZZZ,ZZZ.Z.
           03  WRK-HEIKIN2              PIC 9(07).
           03  WRK-HEIKIN-Z2            PIC Z,ZZZ,ZZZ.
           03  WRK-HEIKIN3              PIC 9(09).
           03  WRK-HEIKIN-Z3            PIC ZZ,ZZZ,ZZZ.
           03  WRK-GKENNAI              PIC 9(06).
           03  WRK-GKENGAI              PIC 9(06).
      *
           03  WRK-Z2              PIC ZZ.
           03  WRK-Z3              PIC ZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z6              PIC ZZ,ZZZ.
           03  WRK-Z7              PIC ZZZ,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z10             PIC ZZ,ZZZ,ZZZ.
           03  WRK-Z11             PIC ZZZ,ZZZ,ZZZ.
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-PATH            PIC X(50).
           03  WRK-PATH2               PIC X(50).
           03  WRK-HKNNUM              PIC X(03).
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
           03  WRK-HKNJANUM        PIC  X(08).
           03  WRK-HKNJANAME       PIC  X(08).
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
           03  CNT-IN              PIC 9(02).
      *
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-SRYYM     PIC X(06).
           03  KEY-OLD.
               05  KEY-O-SRYYM     PIC X(06).
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
      *    医療機関住所編集情報（２）
           COPY    "CPSK1903.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
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
           INITIALIZE                  KEY-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  RECEERR
      *
           DISPLAY  "START"
      *
           PERFORM 100-DBOPEN-SEC
      *
      *###（地方公費仕様）
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
           MOVE    "SOKATU4100"    TO  JOB-PGID
           MOVE    "国保総括表"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
      *##（地方公費仕様：LNK-PRTKANRI-TERMID)
           MOVE    "SKT4100"         TO  SKT4100TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT4100TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT4100TP-HOSPNUM
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  SKT4100TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (3)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT4100TP-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEERR
      *
      *###（地方公費仕様)
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SRYYMD (1:6)
           MOVE    "01"                TO  WRK-SRYYMD (7:2)
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-SRYYMWH 
      *##
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
      *****
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
      *****
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
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           MOVE    "SOKATU4100"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    優先保険者番号
           MOVE    "YUSEN"                   TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           MOVE    WRK-OPTION(1)(1:6)        TO  WRK-YUSEN
           DISPLAY "優先保険者番号="  WRK-YUSEN
      *
      *    診療科
           MOVE    "SRYKA"                   TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  3
             IF  WRK-OPTION(IDX)(1:2)  =  "※"
               MOVE    SPACE                 TO  WRK-SRYKANAME(IDX)
             ELSE
               MOVE    WRK-OPTION(IDX)(1:20) TO  WRK-SRYKANAME(IDX)
             END-IF
           END-PERFORM
           DISPLAY "診療科１="  WRK-SRYKANAME(1)
           DISPLAY "診療科２="  WRK-SRYKANAME(2)
           DISPLAY "診療科３="  WRK-SRYKANAME(3)
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
      
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN     OUTPUT  SKT4100TMP-FILE
           CLOSE            SKT4100TMP-FILE
           OPEN     I-O     SKT4100TMP-FILE
      *
           MOVE     ZERO    TO      FLG-UPD
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
           CLOSE   UPD-FILE
           CLOSE   SKT4100TMP-FILE
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
           DISPLAY "*** SOKATU4100 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU4100 END ***"
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *
      *###（地方公費仕様）
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
           INITIALIZE       SKT4100TMP-REC
           INITIALIZE       SKT4100TMP-HKNJANUM
      *
           IF    RECE10-HKNJANUM(7:2)   NOT  =  SPACE
                 AND  RECE10-HKNJANUM(1:2)   =  "39"
      *    後期高齢者は県単位でまとめる
             PERFORM  20011-KOUIKI-SEC
             MOVE     WRK-HKNJANUM    TO
                              SKT4100TMP-HKNJANUM
             MOVE     WRK-HKNJANAME   TO 
                              SKT4100TMP-HKNJANAME
             DISPLAY "WRK-HKNJANUM"WRK-HKNJANUM
             DISPLAY "★SKT4100TMP-HKNJANUM"SKT4100TMP-HKNJANUM
             DISPLAY "WRK-HKNJANAME"WRK-HKNJANAME
             DISPLAY "★SKT4100TMP-HKNJANAME"SKT4100TMP-HKNJANAME
           ELSE
             IF       RECE10-HKNJANUM(7:2)  =     SPACE
                      MOVE    RECE10-HKNJANUM(1:6)    TO
                              SKT4100TMP-HKNJANUM(1:6)
             ELSE
                      MOVE    RECE10-HKNJANUM(3:6)    TO
                              SKT4100TMP-HKNJANUM(1:6)
             END-IF
           END-IF
      *    政令指定都市は代表番号でまとめる
           PERFORM  20011-SEIREI-SEC
      *
      *    県外→県内市町村→県内扱い国保組合→県外後期→県内後期の順で印字する
           IF    WRK-YUSEN        =  SKT4100TMP-HKNJANUM
             MOVE          1   TO    SKT4100TMP-SYUTURYOKU
           ELSE
             IF     RECE10-PREFKBN            =  "1"
               IF   SKT4100TMP-HKNJANUM(3:1)  =  "3"
                 MOVE  4     TO     SKT4100TMP-SYUTURYOKU
               ELSE
                 MOVE  3     TO     SKT4100TMP-SYUTURYOKU
               END-IF
             ELSE
                 MOVE  2     TO     SKT4100TMP-SYUTURYOKU
             END-IF
           END-IF
             IF   SKT4100TMP-HKNJANAME  NOT =  SPACE
                 DISPLAY "△後期高齢"SKT4100TMP-HKNJANAME
               IF  RECE10-PREFKBN  =  "1"
                 MOVE  6     TO     SKT4100TMP-SYUTURYOKU
               ELSE
                 MOVE  5     TO     SKT4100TMP-SYUTURYOKU
               END-IF
             END-IF
      *
      *法定外給付は法定内給付と分ける
           MOVE   ZERO     TO     SKT4100TMP-KYURATE
           IF  RECE10-SRYYM  >=  WRK-CONS-SRYYM-200804
             IF     RECE10-RJNHKNNUM          =    SPACE   AND
                    RECE10-AGE                <    70      AND
                    RECE10-FLG(1)         NOT =    1       AND
                    SKT4100TMP-HKNJANUM(3:1)  =   "3"      AND
                    RECE10-KYURATE       NOT  =    7
               MOVE     RECE10-KYURATE        TO   SKT4100TMP-KYURATE
             END-IF
           ELSE
             IF     RECE10-RJNHKNNUM          =    SPACE   AND
                    RECE10-AGE                <    70      AND
                    RECE10-AGE                >    2       AND
                    SKT4100TMP-HKNJANUM(3:1)  =   "3"      AND
                    RECE10-KYURATE       NOT  =    7
               MOVE     RECE10-KYURATE        TO   SKT4100TMP-KYURATE
             END-IF
           END-IF
      *
      *    在宅は一般と分ける
           MOVE    RECE10-ZAITAKU           TO   SKT4100TMP-ZAITAKU
      *
           PERFORM  900-SKTTMP-READ-SEC
      *
           PERFORM  210-HKN-SYUKEI-SEC
      *
      *    集計の対象にならなかったとき
           IF      FLG-UPD             =   1
               PERFORM 230-UPD-FILE-HENSYU-SEC
      *
               IF  FLG-KOUKI  =  ZERO
                  PERFORM   900-SEIKYU-READ-SEC
               ELSE
                  PERFORM   950-SEIKYU-READ-SEC
               END-IF
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
      *    広域連合・代表番号セット
      *****************************************************************
       20011-KOUIKI-SEC              SECTION.
      *
           DISPLAY "20011-KOUIKI-SEC--"RECE10-HKNJANUM
           EVALUATE  RECE10-HKNJANUM(3:2)
      *      北海道
             WHEN  "01"
                 MOVE  "39010004"  TO  WRK-HKNJANUM
                 MOVE  "北海道"  TO  WRK-HKNJANAME
      *      青森県
             WHEN  "02"
                 MOVE  "39020003"  TO  WRK-HKNJANUM
                 MOVE  "青森県"  TO  WRK-HKNJANAME
      *      岩手県
             WHEN  "03"
                 MOVE  "39030002"  TO  WRK-HKNJANUM
                 MOVE  "岩手県"  TO  WRK-HKNJANAME
      *      宮城県
             WHEN  "04"
                 MOVE  "39040001"  TO  WRK-HKNJANUM
                 MOVE  "宮城県"  TO  WRK-HKNJANAME
      *      秋田県
             WHEN  "05"
                 MOVE  "39050000"  TO  WRK-HKNJANUM
                 MOVE  "秋田県"  TO  WRK-HKNJANAME
      *      山形県
             WHEN  "06"
                 MOVE  "39060009"  TO  WRK-HKNJANUM
                 MOVE  "山形県"  TO  WRK-HKNJANAME
      *      福島県
             WHEN  "07"
                 MOVE  "39070008"  TO  WRK-HKNJANUM
                 MOVE  "福島県"  TO  WRK-HKNJANAME
      *      茨城県
             WHEN  "08"
                 MOVE  "39080007"  TO  WRK-HKNJANUM
                 MOVE  "茨城県"  TO  WRK-HKNJANAME
      *      栃木県
             WHEN  "09"
                 MOVE  "39090006"  TO  WRK-HKNJANUM
                 MOVE  "栃木県"  TO  WRK-HKNJANAME
      *      群馬県
             WHEN  "10"
                 MOVE  "39100003"  TO  WRK-HKNJANUM
                 MOVE  "群馬県"  TO  WRK-HKNJANAME
      *      埼玉県
             WHEN  "11"
                 MOVE  "39110002"  TO  WRK-HKNJANUM
                 MOVE  "埼玉県"  TO  WRK-HKNJANAME
      *      千葉県
             WHEN  "12"
                 MOVE  "39120001"  TO  WRK-HKNJANUM
                 MOVE  "千葉県"  TO  WRK-HKNJANAME
      *      東京都
             WHEN  "13"
                 MOVE  "39130000"  TO  WRK-HKNJANUM
                 MOVE  "東京都"  TO  WRK-HKNJANAME
      *      神奈川県
             WHEN  "14"
                 MOVE  "39140009"  TO  WRK-HKNJANUM
                 MOVE  "神奈川県"  TO  WRK-HKNJANAME
      *      新潟県
             WHEN  "15"
                 MOVE  "39150008"  TO  WRK-HKNJANUM
                 MOVE  "新潟県"  TO  WRK-HKNJANAME
      *      富山県
             WHEN  "16"
                 MOVE  "39160007"  TO  WRK-HKNJANUM
                 MOVE  "富山県"  TO  WRK-HKNJANAME
      *      石川県
             WHEN  "17"
                 MOVE  "39170006"  TO  WRK-HKNJANUM
                 MOVE  "石川県"  TO  WRK-HKNJANAME
      *      福井県
             WHEN  "18"
                 MOVE  "39180005"  TO  WRK-HKNJANUM
                 MOVE  "福井県"  TO  WRK-HKNJANAME
      *      山梨県
             WHEN  "19"
                 MOVE  "39190004"  TO  WRK-HKNJANUM
                 MOVE  "山梨県"  TO  WRK-HKNJANAME
      *      長野県
             WHEN  "20"
                 MOVE  "39200001"  TO  WRK-HKNJANUM
                 MOVE  "長野県"  TO  WRK-HKNJANAME
      *      岐阜県
             WHEN  "21"
                 MOVE  "39210000"  TO  WRK-HKNJANUM
                 MOVE  "岐阜県"  TO  WRK-HKNJANAME
      *      静岡県
             WHEN  "22"
                 MOVE  "39220009"  TO  WRK-HKNJANUM
                 MOVE  "静岡県"  TO  WRK-HKNJANAME
      *      愛知県
             WHEN  "23"
                 MOVE  "39230008"  TO  WRK-HKNJANUM
                 MOVE  "愛知県"  TO  WRK-HKNJANAME
      *      三重県
             WHEN  "24"
                 MOVE  "39240007"  TO  WRK-HKNJANUM
                 MOVE  "三重県"  TO  WRK-HKNJANAME
      *      滋賀県
             WHEN  "25"
                 MOVE  "39250006"  TO  WRK-HKNJANUM
                 MOVE  "滋賀県"  TO  WRK-HKNJANAME
      *      京都府
             WHEN  "26"
                 MOVE  "39260005"  TO  WRK-HKNJANUM
                 MOVE  "京都府"  TO  WRK-HKNJANAME
      *      大阪府
             WHEN  "27"
                 MOVE  "39270004"  TO  WRK-HKNJANUM
                 MOVE  "大阪府"  TO  WRK-HKNJANAME
      *      兵庫県
             WHEN  "28"
                 MOVE  "39280003"  TO  WRK-HKNJANUM
                 MOVE  "兵庫県"  TO  WRK-HKNJANAME
      *      奈良県
             WHEN  "29"
                 MOVE  "39290002"  TO  WRK-HKNJANUM
                 MOVE  "奈良県"  TO  WRK-HKNJANAME
      *      和歌山県
             WHEN  "30"
                 MOVE  "39300009"  TO  WRK-HKNJANUM
                 MOVE  "和歌山県"  TO  WRK-HKNJANAME
      *      鳥取県
             WHEN  "31"
                 MOVE  "39310008"  TO  WRK-HKNJANUM
                 MOVE  "鳥取県"  TO  WRK-HKNJANAME
      *      島根県
             WHEN  "32"
                 MOVE  "39320007"  TO  WRK-HKNJANUM
                 MOVE  "島根県"  TO  WRK-HKNJANAME
      *      岡山県
             WHEN  "33"
                 MOVE  "39330006"  TO  WRK-HKNJANUM
                 MOVE  "岡山県"  TO  WRK-HKNJANAME
      *      広島県
             WHEN  "34"
                 MOVE  "39340005"  TO  WRK-HKNJANUM
                 MOVE  "広島県"  TO  WRK-HKNJANAME
      *      山口県
             WHEN  "35"
                 MOVE  "39350004"  TO  WRK-HKNJANUM
                 MOVE  "山口県"  TO  WRK-HKNJANAME
      *      徳島県
             WHEN  "36"
                 MOVE  "39360003"  TO  WRK-HKNJANUM
                 MOVE  "徳島県"  TO  WRK-HKNJANAME
      *      香川県
             WHEN  "37"
                 MOVE  "39370002"  TO  WRK-HKNJANUM
                 MOVE  "香川県"  TO  WRK-HKNJANAME
      *      愛媛県
             WHEN  "38"
                 MOVE  "39380001"  TO  WRK-HKNJANUM
                 MOVE  "愛媛県"  TO  WRK-HKNJANAME
      *      高知県
             WHEN  "39"
                 MOVE  "39390000"  TO  WRK-HKNJANUM
                 MOVE  "高知県"  TO  WRK-HKNJANAME
      *      福岡県
             WHEN  "40"
                 MOVE  "39400007"  TO  WRK-HKNJANUM
                 MOVE  "福岡県"  TO  WRK-HKNJANAME
      *      佐賀県
             WHEN  "41"
                 MOVE  "39410006"  TO  WRK-HKNJANUM
                 MOVE  "佐賀県"  TO  WRK-HKNJANAME
      *      長崎県
             WHEN  "42"
                 MOVE  "39420005"  TO  WRK-HKNJANUM
                 MOVE  "長崎県"  TO  WRK-HKNJANAME
      *      熊本県
             WHEN  "43"
                 MOVE  "39430004"  TO  WRK-HKNJANUM
                 MOVE  "熊本県"  TO  WRK-HKNJANAME
      *      大分県
             WHEN  "44"
                 MOVE  "39440003"  TO  WRK-HKNJANUM
                 MOVE  "大分県"  TO  WRK-HKNJANAME
      *      宮崎県
             WHEN  "45"
                 MOVE  "39450002"  TO  WRK-HKNJANUM
                 MOVE  "宮崎県"  TO  WRK-HKNJANAME
      *      鹿児島県
             WHEN  "46"
                 MOVE  "39460001"  TO  WRK-HKNJANUM
                 MOVE  "鹿児島県"  TO  WRK-HKNJANAME
      *      沖縄県
             WHEN  "47"
                 MOVE  "39470000"  TO  WRK-HKNJANUM
                 MOVE  "沖縄県"  TO  WRK-HKNJANAME
           END-EVALUATE
      *
           .
       20011-KOUIKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    政令指定都市の代表番号集計
      *****************************************************************
       20011-SEIREI-SEC              SECTION.
      *
      *    札幌市の場合、代表番号セット
           IF  SKT4100TMP-HKNJANUM (1:3) = "014"
             MOVE  "010017"  TO  SKT4100TMP-HKNJANUM
           END-IF
      *
      *    仙台市の場合、代表番号セット
           IF  SKT4100TMP-HKNJANUM (1:3) = "044"
             MOVE  "040014"  TO  SKT4100TMP-HKNJANUM
           END-IF
      *
      *    さいたま市の場合、代表番号セット
           IF  SKT4100TMP-HKNJANUM (1:3) = "114"
             MOVE  "114009"  TO  SKT4100TMP-HKNJANUM
           END-IF
      *
      *    千葉市の場合、代表番号セット
           IF  SKT4100TMP-HKNJANUM (1:3) = "124"
             MOVE  "124008"  TO  SKT4100TMP-HKNJANUM
           END-IF
      *
      *    横浜市の場合、代表番号セット
           IF  SKT4100TMP-HKNJANUM (1:3) = "144"
             MOVE  "144006"  TO  SKT4100TMP-HKNJANUM
           END-IF
      *
      *    川崎市の場合、代表番号セット
           IF  SKT4100TMP-HKNJANUM (1:3) = "145"
             MOVE  "145003"  TO  SKT4100TMP-HKNJANUM
           END-IF
      *
      *    名古屋市の場合、代表番号セット
           IF  SKT4100TMP-HKNJANUM (1:3) = "234"
             MOVE  "234005"  TO  SKT4100TMP-HKNJANUM
           END-IF
      *
      *    京都市の場合、代表番号セット
           IF  SKT4100TMP-HKNJANUM (1:3) = "264"
             MOVE  "264002"  TO  SKT4100TMP-HKNJANUM
           END-IF
      *
      *    大阪市の場合、代表番号セット
           IF  SKT4100TMP-HKNJANUM (1:3) = "274"
             MOVE  "274001"  TO  SKT4100TMP-HKNJANUM
           END-IF
      *
      *    神戸市の場合、代表番号セット
           IF  SKT4100TMP-HKNJANUM (1:3) = "284"
             MOVE  "284000"  TO  SKT4100TMP-HKNJANUM
           END-IF
      *
      *    広島市の場合、代表番号セット
           IF  SKT4100TMP-HKNJANUM (1:3) = "344"
             MOVE  "344002"  TO  SKT4100TMP-HKNJANUM
           END-IF
      *
      *    北九州市の場合、代表番号セット
           IF  SKT4100TMP-HKNJANUM (1:3) = "404"
             MOVE  "404004"  TO  SKT4100TMP-HKNJANUM
           END-IF
      *
      *    福岡市の場合、代表番号セット
           IF  SKT4100TMP-HKNJANUM (1:3) = "405"
             MOVE  "405001"  TO  SKT4100TMP-HKNJANUM
           END-IF
      *
           .
       20011-SEIREI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険・後期高齢（老人保健）分集計処理
      *****************************************************************
       210-HKN-SYUKEI-SEC          SECTION.
      *
      *    退職者かどうか
           IF  RECE10-HKNNUM  =  "060"  OR  "068"
               MOVE  1  TO  IDX1
           ELSE
                 MOVE  2  TO   IDX1
           END-IF
      *
      *    後期高齢または老人保健か
           IF  RECE10-RJNHKNNUM  NOT  =  SPACE
               OR  FLG-KOUKI          =  1
               MOVE  3      TO   IDX1
           END-IF
      *
           PERFORM  2101-SYUKEI-SEC
      *
           IF  FLG-SKT4100TMP  =  1
               WRITE    SKT4100TMP-REC
           ELSE
               REWRITE  SKT4100TMP-REC
           END-IF
      *
      *    合計用ファイル編集
           PERFORM  2101-GOKEI-HEN-SEC
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
      *    入院・外来
           IF  RECE10-NYUGAIKBN  =  "1"
               MOVE  1  TO  IDX2
           ELSE
               MOVE  2  TO  IDX2
           END-IF
      *    療養の給付：件数
           ADD   1                  TO  SKT4100TMP-KENSU (IDX1 IDX2)
      *    療養の給付：日数
           ADD   RECE10-JNISSU (1)  TO  SKT4100TMP-NISSU (IDX1 IDX2)
      *    療養の給付：点数
           COMPUTE SKT4100TMP-TENSU(IDX1 IDX2)  =
                   SKT4100TMP-TENSU(IDX1 IDX2)  +
                   RECE10-TOTALTEN(1)
      *    合計計算
                 IF  IDX1 = 3
                   MOVE  2  TO  IDX4
                 ELSE
                   MOVE  1  TO  IDX4
                 END-IF
      *    合計ファイルの時のみ加算
           IF     SKT4100TMP-SYUTURYOKU   =   ZERO
                  AND  FLG-KOUKI          =   ZERO
             IF  RECE10-PREFKBN  =  "1"
               ADD    1       TO    SKT4100TMP-KENNAI
             ELSE
               ADD    1       TO    SKT4100TMP-KENGAI
             END-IF
           END-IF
           IF     SKT4100TMP-SYUTURYOKU   =   ZERO
                  AND  FLG-KOUKI          =   1
             IF  RECE10-PREFKBN  =  "1"
               ADD    1       TO    SKT4100TMP-KOUKENNAI
             ELSE
               ADD    1       TO    SKT4100TMP-KOUKENGAI
             END-IF
           END-IF
      *
           MOVE    1               TO  FLG-UPD
           .
       2101-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    合計用ファイル編集処理
      *****************************************************************
       2101-GOKEI-HEN-SEC         SECTION.
      *
           INITIALIZE     SKT4100TMP-REC
      *
      *    KEY設定
           MOVE     ZERO             TO     SKT4100TMP-SYUTURYOKU
      *
           PERFORM  900-SKTTMP-READ-SEC
      *
      *    データ加算
           IF     FLG-KOUKI  NOT  =  ZERO
             MOVE     2              TO     IDX1
           ELSE
             MOVE     1              TO     IDX1
           END-IF
      *
           PERFORM  2101-SYUKEI-SEC
      *
           IF  FLG-SKT4100TMP  =  1
               WRITE    SKT4100TMP-REC
           ELSE
               REWRITE  SKT4100TMP-REC
           END-IF
      *
           .
       2101-GOKEI-HEN-EXT.
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
           OPEN     INPUT       SKT4100TMP-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =   1
             MOVE  KEY-NEW   TO   KEY-OLD
      *
      *    帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
               PERFORM 320-BODY-HEN-SEC
      *    帳票印刷処理
               PERFORM 340-PRINT-OUT-SEC
           END-PERFORM
      *
           CLOSE    SKT4100TMP-FILE
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
           INITIALIZE                    SKT4100
      *    診療年月
           MOVE    WRK-SRYYMWH           TO  SKT4100-SRYYM
      *    診療科
           MOVE    WRK-SRYKANAME(1)      TO  SKT4100-SRYKA(1)
           MOVE    WRK-SRYKANAME(2)      TO  SKT4100-SRYKA(2)
           MOVE    WRK-SRYKANAME(3)      TO  SKT4100-SRYKA(3)
      *    医療機関住所
           MOVE    WRK-HOSPADRS          TO  SKT4100-ADRS
           INSPECT SKT4100-ADRS          REPLACING ALL "  " BY "　"
      *    医療機関名
           MOVE    WRK-HOSPNAME          TO  SKT4100-HOSPNAME
           INSPECT SKT4100-HOSPNAME      REPLACING ALL "  " BY "　"
      *    電話番号
           MOVE    WRK-HOSPTEL           TO  SKT4100-TEL
      *    開設者名
           MOVE    WRK-KAISETUNAME       TO  SKT4100-KAISETUNAME
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN      TO  SKT4100-HOSPCDN
      *    特別療養費
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE  "特別療養費"          TO  SKT4100-TOKUBETU
           END-IF
      *
      *    合計用ファイルの場合、合計を設定
           IF      SKT4100TMP-SYUTURYOKU  =  ZERO
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
               PERFORM  VARYING  IDX3  FROM  1  BY  1
                        UNTIL    IDX3  >     2
      *    件数
                 MOVE  SKT4100TMP-KENSU (IDX2 IDX3)  TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT4100-GKENSU(IDX2 IDX3)
      *    日数
                 MOVE  SKT4100TMP-NISSU (IDX2 IDX3)  TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT4100-GNISSU(IDX2 IDX3)
      *    点数
                 MOVE  SKT4100TMP-TENSU (IDX2 IDX3)  TO  WRK-Z10
                 MOVE  WRK-Z10  TO  SKT4100-GTENSU(IDX2 IDX3)
      *    １件当り点数
                 MOVE     ZERO         TO      WRK-HEIKIN3
                 COMPUTE  WRK-HEIKIN3  ROUNDED =
                                        SKT4100TMP-TENSU(IDX2 IDX3)
                                     /  SKT4100TMP-KENSU(IDX2 IDX3)
                 MOVE     WRK-HEIKIN3         TO  WRK-HEIKIN-Z3
                 MOVE     WRK-HEIKIN-Z3   TO  SKT4100-ATENSU(IDX2 IDX3)
               END-PERFORM
             END-PERFORM
             MOVE    SKT4100TMP-KENNAI    TO  WRK-Z6
             MOVE    WRK-Z6               TO  SKT4100-GKENNAI
             MOVE    SKT4100TMP-KENGAI    TO  WRK-Z6
             MOVE    WRK-Z6               TO  SKT4100-GKENGAI
      *    後期高齢 県内・県外総件数
             MOVE    SKT4100TMP-KOUKENNAI TO  WRK-Z6
             MOVE    WRK-Z6               TO  SKT4100-GKOUKENNAI
             MOVE    SKT4100TMP-KOUKENGAI TO  WRK-Z6
             MOVE    WRK-Z6               TO  SKT4100-GKOUKENGAI
      *
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
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
           PERFORM  VARYING  IDX1  FROM   1   BY  1
                    UNTIL    IDX1  >      10
                    OR       FLG-PRINT  =  1
                    OR       KEY-NEW  NOT  =  KEY-OLD
      *    保険者名
             DISPLAY "SKT4100TMP-HKNJANAME"SKT4100TMP-HKNJANAME
             IF  SKT4100TMP-HKNJANAME  NOT  =  SPACE
                 MOVE  SPACE  TO   HKNJA-HKNJANAME
                 STRING  SKT4100TMP-HKNJANAME  DELIMITED BY SPACE
                         "広域連合"            DELIMITED BY SIZE
                         INTO                  HKNJA-HKNJANAME
                 END-STRING
             ELSE
                 PERFORM  920-HKNJAINF-SET-SEC
             END-IF
      *    在宅の場合は保険者名の頭に在を印字する
             IF    SKT4100TMP-ZAITAKU    =  SPACE
               MOVE  HKNJA-HKNJANAME TO  SKT4100-HKNJANAME (IDX1)
             ELSE
               STRING  "(在)"             DELIMITED BY SIZE
                        HKNJA-HKNJANAME   DELIMITED BY SPACE
                        INTO              SKT4100-HKNJANAME (IDX1)
               END-STRING
             END-IF
      *    国保組合の法定外給付は給付割合を印字する
             IF     SKT4100TMP-KYURATE     >    ZERO
               MOVE  SKT4100TMP-KYURATE    TO   WRK-Z2
               STRING  "("                 DELIMITED BY SIZE
                       WRK-Z2              DELIMITED BY SIZE
                       "割"                DELIMITED BY SIZE
                       ")"                 DELIMITED BY SIZE
                       INTO                SKT4100-KYURATE(IDX1)
               END-STRING
             END-IF
      *
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     3
               PERFORM  VARYING  IDX3  FROM  1  BY  1
                        UNTIL    IDX3  >     2
      *    件数
                 MOVE  SKT4100TMP-KENSU (IDX2 IDX3)  TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT4100-KENSU(IDX1 IDX2 IDX3)
      *    日数
                 MOVE  SKT4100TMP-NISSU (IDX2 IDX3)  TO  WRK-Z5
                 MOVE  WRK-Z5   TO  SKT4100-NISSU(IDX1 IDX2 IDX3)
      *    点数
                 MOVE  SKT4100TMP-TENSU (IDX2 IDX3)  TO  WRK-Z10
                 MOVE  WRK-Z10  TO  SKT4100-TENSU(IDX1 IDX2 IDX3)
               END-PERFORM
             END-PERFORM
      *    一時ファイル読込
             PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       330-SUM-HEN-SEC      SECTION.
      *
      *    合計処理
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     2
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
               MOVE  WRK-GKENSU (IDX1 IDX2)  TO  WRK-Z5
               MOVE  WRK-Z5          TO  SKT4100-GKENSU (IDX1 IDX2)
               MOVE  WRK-GNISSU (IDX1 IDX2)  TO  WRK-Z5
               MOVE  WRK-Z5          TO  SKT4100-GNISSU (IDX1 IDX2)
               MOVE  WRK-GTENSU (IDX1 IDX2)  TO  WRK-Z10
               MOVE  WRK-Z10         TO  SKT4100-GTENSU (IDX1 IDX2)
             END-PERFORM
           END-PERFORM
      *    １件あたり処理
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >     2
             PERFORM  VARYING  IDX2  FROM  1  BY  1
                      UNTIL    IDX2  >     2
               INITIALIZE WRK-HEIKIN3
               INITIALIZE WRK-HEIKIN-Z3
               DISPLAY "合計点数="  WRK-GTENSU(IDX1 IDX2)
               DISPLAY "合計件数="  WRK-GKENSU(IDX1 IDX2)
               COMPUTE  WRK-HEIKIN3  ROUNDED =  WRK-GTENSU(IDX1 IDX2)
                                             /  WRK-GKENSU(IDX1 IDX2)
               DISPLAY "平均点数="  WRK-HEIKIN3
               MOVE     WRK-HEIKIN3         TO  WRK-HEIKIN-Z3
               MOVE     WRK-HEIKIN-Z3   TO  SKT4100-ATENSU(IDX1 IDX2)
             END-PERFORM
           END-PERFORM
      *    県内・県外総件数
           MOVE    WRK-GKENNAI          TO  WRK-Z6
           MOVE    WRK-Z6               TO  SKT4100-GKENNAI
           MOVE    WRK-GKENGAI          TO  WRK-Z6
           MOVE    WRK-Z6               TO  SKT4100-GKENGAI
           .
       330-SUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       340-PRINT-OUT-SEC                SECTION.
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
           MOVE   "SKT4100.red"        TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN  ALSO  WRK-PARA-TOKUBETU
             WHEN  "1"  ALSO  "1"
               MOVE  "国保総括表"                TO  SPRT-TITLE
             WHEN  "2"  ALSO  "1"
               MOVE  "国保総括表［返戻分］"      TO  SPRT-TITLE
             WHEN  "1"  ALSO  "2"
               MOVE  "国保総括表［特別療養費］"  TO  SPRT-TITLE
             WHEN  "2"  ALSO  "2"
               MOVE  "国保総括表［返戻分］［特別療養費］" TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT4100             TO  SPRT-PRTDATA
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
       340-PRINT-OUT-EXT.
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
      *###（地方公費仕様）
                   MOVE    LNK-PRTKANRI-SRYYM
                                           TO  RECE10-SKYYM
                   MOVE    LNK-PRTKANRI-SKYYMD
                                           TO  RECE10-UPDYMD
      *
                   MOVE    RECE10-REC             TO  MCPDATA-REC
                   MOVE    "DBUPDATE"             TO  MCP-FUNC
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
                       MOVE  "*** SOKATU1410 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                   MOVE  "*** SOKATU1410 SEIKYU INVALID ***"
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
      *  保険者情報　取得
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
           MOVE  SPA-HOSPNUM               TO  HKNJA-HOSPNUM
           MOVE  SKT4100TMP-HKNJANUM       TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC              TO  MCPDATA-REC
           MOVE  "DBSELECT"                TO  MCP-FUNC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF  MCP-RC  =  ZERO
             MOVE    "tbl_hknjainf"      TO  MCP-TABLE
             MOVE    "key"               TO  MCP-PATHNAME
             PERFORM  925-HKNJAINF-READ-SEC
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"        TO  MCP-FUNC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
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
           MOVE  "DBFETCH"         TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
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
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
      *
      *###（地方公費仕様）
           MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
           MOVE   LNK-PRTKANRI-SRYYM    TO    RECE10-SKYYM.
      *
           MOVE    SPACE               TO  RECE10-XXSTHKNJANUM
           MOVE    "99999999"          TO  RECE10-XXEDHKNJANUM
           IF  WRK-PARA-TOKUBETU  =  "2"
             MOVE    000000            TO  RECE10-SRYYM
             MOVE    999999            TO  RECE10-XXSRYYM
           END-IF
      *
           MOVE    RECE10-REC                 TO  MCPDATA-REC
           MOVE    "DBSELECT"                 TO  MCP-FUNC
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
               MOVE    SPACE              TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"         TO  RECE10-XXEDHKNJANUM
      *
           MOVE    RECE10-REC              TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    WRK-PATH2               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           DISPLAY "950-SEIKYU-SELECT-SEC"MCP-RC
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
           DISPLAY "950-SEIKYU-READ-SEC"MCP-RC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE10-REC
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
      *        返戻分でオンライン請求は読み飛ばす
               IF      (WRK-PARA-SYOKBN         =  "2")
               AND     (RECE10-HENREI-SIJI-FLG  =  1)
                   GO  TO  950-SEIKYU-READ-SEC
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
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING
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
           READ    SKT4100TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT4100TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT4100TMP
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
      *###（地方公費仕様）
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
           READ  SKT4100TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
               NOT AT END
                   MOVE  SKT4100TMP-SRYYM  TO  KEY-N-SRYYM
           END-READ
           .
       900-TMP-READ-EXT.
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
