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
       PROGRAM-ID.             SOKATU2000.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国保・請求書集計処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/06/20    有村          再作成(2.5.0対応)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev 修正者  日付      内容
      *  01.00.01    内田    07/10/05    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *  01.00.02    内田    08/04/23    帳票様式の変更に伴う修正
      *                                  SPA-AREA初期化
      *  01.00.03    内田    08/05/28    後期高齢者のみでも印字する
      *  01.00.04    内田    08/08/13    後期高齢者のみの場合でも印字する
      *  01.00.05    吉川    09/12/07    レセ電時の紙レセの返戻は、
      *                                  前月分としてまとめて印字する
      *  01.00.06    吉川    10/02/02    兵庫県の広域連合名が印字されないのを修正
      *  01.00.07    吉川    10/08/16    返戻分が2枚以上になる場合、
      *                                  診療年月がおかしくなるのを修正
      *  01.00.08    吉川    10/10/01    オンライン返戻対応
      *                                  特別療養費国保対応
      *  01.00.09    吉川    11/03/07    open-cobol1.0対応
      *  01.00.10    吉川    11/11/10    診療科を５行まで印字できるように修正
      *  01.00.11    吉川    13/01/09    プラグイン対応
      *                                  医療機関情報編集対応
      *  01.00.12    吉川    13/07/02    プログラムオプション対応
      *  01.00.13    吉川    14/08/06    返戻時、再印刷画面のタイトル修正
      *  01.00.14    吉川    14/10/23    ver4.8.0対応（一時ディレクトリ変更）
      *  01.00.15    吉川    15/11/16    医療機関編集情報への開設者名追加対応
      *  01.00.16    萩野    17/12/11    医療機関コード変更時、
      *                                  医療機関コード別の作成処理対応
      *                                  SITEKBN変更（動作に変更なし）
      *  01.00.17    萩野    18/12/12    修正履歴コメント削除
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.

      *    請求書一時ファイル  
           SELECT  SKT2000TMP-FILE ASSIGN    SKT2000TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT2000TMP-KEY
                                   FILE    STATUS  IS  STS-SKT2000.
      *    請求書一時ファイル
           SELECT  SKT2001TMP-FILE ASSIGN    SKT2001TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT2001TMP-KEY
                                   FILE    STATUS  IS  STS-SKT2001.
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
       FD  SKT2000TMP-FILE.
           COPY    "CPSKT2000TMP.INC".
      *    請求書一時ファイル（後期高齢用）
       FD  SKT2001TMP-FILE.
           COPY    "CPSKT2000TMP.INC" REPLACING  //SKT2000TMP//
                                   BY         //SKT2001TMP//.
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
                                   BY         //SKT2000TP//.
      *    請求書一時ファイル 名称領域（後期高齢）
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //SKT2001TP//.
      *
006900*    請求ＤＢ更新用ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEUPD//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *
           COPY    "SKT2000.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT2000         PIC X(02).
           03  STS-SKT2001         PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-SRYKA           PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT2000TMP      PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-PRINT2          PIC 9(01).
           03  FLG-SKT2001TMP      PIC 9(01).
           03  FLG-KOUKI           PIC 9(01).
           03  FLG-RECEDEN         PIC 9(01).
           03  FLG-KOURECEDEN      PIC 9(01).
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
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYOKBN     PIC X(01).
           03  WRK-PARA-STSRYYM    PIC 9(06).
           03  WRK-PARA-EDSRYYM    PIC 9(06).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SEIYMD          PIC X(08).
           03  WRK-SEIYMDWH        PIC X(22).
           03  WRK-SRYYMWH         PIC X(16).
           03  WRK-SRYYMWH2        PIC X(16).
           03  WRK-SRYYMD          PIC X(08).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-CNT             PIC 9(02).
           03  WRK-SUM.
               05  WRK-SKENSU      PIC 9(05).
               05  WRK-STYOKI      PIC 9(05).
           03  WRK-SUM2.
               05  WRK-SKENSU2     PIC 9(05).
               05  WRK-STYOKI2     PIC 9(05).
           03  WRK-SRYKANAME    PIC X(20) OCCURS 5.
      *
           03  WRK-HKNJANUM        PIC X(06).
      *
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z6              PIC ZZ,ZZZ.
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-MAE-INPUT       PIC X(200).
           03  WRK-OUT-INPUT       PIC X(200).
           03  WRK-PATH              PIC X(50).
           03  WRK-PATH2             PIC X(50).
           03  WRK-HKNNUM            PIC X(03).
           03  WRK-KENNAME           PIC X(08).
      *
           03  WRK-OPTION-TBL.
               05  WRK-OPTION      PIC X(100)  OCCURS 100.
           03  WRK-PRGID           PIC X(25).
           03  WRK-KBNCD           PIC X(16).
           03  WRK-YUSEN           PIC X(06).
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
      *    暫定処置
           03  WRK-KENSU           PIC 9(05).
      *    特別療養費件数
           03  WRK-TOKUKENSU       PIC 9(05) OCCURS 2.
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
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-PREFKBN       PIC X(01).
           03  KEY-OLD.
               05  KEY-O-PREFKBN       PIC X(01).
       01  KEY-AREA2.
           03  KEY-NEW2.
               05  KEY-N-PREFKBN2      PIC X(01).
           03  KEY-OLD2.
               05  KEY-O-PREFKBN2      PIC X(01).
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
      *    レセプト・総括印刷情報
           COPY    "CPSK2005.INC".
      *
      *    医療機関情報−レセプト・総括印刷情報（後期高齢）
           COPY    "CPSK200501.INC".
      *
      *    請求管理情報
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
      *    半角チェックサブ
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
           INITIALIZE                  IDX-AREA
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
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-SYOKBN
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
           MOVE    "SOKATU2000"    TO  JOB-PGID
           MOVE    "国保総括表"    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    サブプログラムより一時ファイル名編集
           INITIALIZE                      SGETTEMP-AREA
           MOVE    "SKT2000"         TO  SKT2000TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT2000TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT2000TP-HOSPNUM
      *
           MOVE    "SKT2001"           TO  SKT2001TP-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  SKT2001TP-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  SKT2001TP-HOSPNUM
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *
           MOVE  SKT2000TP-BASENAME      TO  SGETTEMP-BASENAMES (1)
           MOVE  SKT2001TP-BASENAME      TO  SGETTEMP-BASENAMES (2)
           MOVE  RECEUPD-BASENAME        TO  SGETTEMP-BASENAMES (3)
           MOVE  RECEERR                 TO  SGETTEMP-BASENAMES (4)
           CALL  "ORCSGETTEMP"           USING
                                         SGETTEMP-AREA
           MOVE  SGETTEMP-FULLNAMES (1)  TO  SKT2000TP-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (2)  TO  SKT2001TP-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (3)  TO  RECEUPD-FULLNAME
           MOVE  SGETTEMP-FULLNAMES (4)  TO  RECEERR
      *
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
      *    レセプト・総括印刷情報 レセ電か
           INITIALIZE                  SYS-2005-REC
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                       TO  SYS-2005-STYUKYMD(1:6)
                                           SYS-2005-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-2005-STYUKYMD(7:2)
                                           SYS-2005-EDYUKYMD(7:2)
           MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2005-REC
               MOVE    SYS-2005-KKHRECEKBN TO  FLG-RECEDEN
           ELSE
               MOVE    ZERO                TO  FLG-RECEDEN
           END-IF 
      *
      *    医療機関情報−レセプト・総括印刷情報（後期高齢）
           INITIALIZE                     SYS-200501-REC
           MOVE     "2005"        TO      SYS-200501-KANRICD
           MOVE     "01"          TO      SYS-200501-KBNCD
           MOVE     LNK-PRTKANRI-SRYYM(1:6)
                                  TO      SYS-200501-STYUKYMD(1:6)
                                          SYS-200501-EDYUKYMD(1:6)
           MOVE     "01"          TO      SYS-200501-STYUKYMD(7:2)
                                          SYS-200501-EDYUKYMD(7:2)
           MOVE     SPA-HOSPNUM   TO      SYS-200501-HOSPNUM
           MOVE     SYS-200501-REC  TO      MCPDATA-REC
           PERFORM  910-SYSKANRI-INV-SEC
           IF       FLG-SYSKANRI       =  ZERO
               MOVE    MCPDATA-REC   TO    SYS-200501-REC
               MOVE    SYS-200501-KOUIKIRECEKBN TO  FLG-KOURECEDEN
           ELSE
               MOVE    ZERO                     TO  FLG-KOURECEDEN
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
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
           OPEN    OUTPUT  UPD-FILE
      *
           EVALUATE    WRK-PARA-SYOKBN
           WHEN    "2"
                   MOVE    "key37"                  TO  WRK-PATH
                   MOVE    "key65"                  TO  WRK-PATH2
           WHEN    "1"
                   MOVE    "key38"                  TO  WRK-PATH
                   MOVE    "key66"                  TO  WRK-PATH2
           END-EVALUATE
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
           MOVE    "SOKATU2000"              TO  WRK-PRGID
      *
      *    プログラムオプション取得
      *    診療科
           MOVE    "SRYKA"                   TO  WRK-KBNCD
           PERFORM 1001-OPTION-SET-SEC
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >  5
             IF  WRK-OPTION(IDX)(1:2)  =  "※"
               MOVE    SPACE                 TO  WRK-SRYKANAME(IDX)
             ELSE
               MOVE    WRK-OPTION(IDX)(1:20) TO  WRK-SRYKANAME(IDX)
             END-IF
           END-PERFORM
           DISPLAY "診療科１="  WRK-SRYKANAME(1)
           DISPLAY "診療科２="  WRK-SRYKANAME(2)
           DISPLAY "診療科３="  WRK-SRYKANAME(3)
           DISPLAY "診療科４="  WRK-SRYKANAME(4)
           DISPLAY "診療科５="  WRK-SRYKANAME(5)
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
           OPEN    OUTPUT  SKT2000TMP-FILE
           CLOSE           SKT2000TMP-FILE
           OPEN    I-O     SKT2000TMP-FILE
           OPEN    OUTPUT  SKT2001TMP-FILE
           CLOSE           SKT2001TMP-FILE
           OPEN    I-O     SKT2001TMP-FILE
      *
           MOVE    ZERO    TO      FLG-UPD
      *
           PERFORM 2001-KOKUHO-SYUKEI-SEC
                   UNTIL   FLG-END =   1
      *
           PERFORM  900-SEIKYU-CLOSE-SEC
           MOVE     ZERO    TO        FLG-END
           MOVE     1       TO        FLG-KOUKI
           MOVE    "039"    TO        WRK-HKNNUM
           PERFORM  950-SEIKYU-SELECT-SEC
           PERFORM  2002-KOUKI-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
           PERFORM  950-SEIKYU-CLOSE-SEC
      *
      *    特別療養費国保集計
           MOVE     ZERO    TO        WRK-TOKUKENSU(1)
           MOVE     ZERO    TO        WRK-TOKUKENSU(2)
      *
           MOVE     ZERO    TO        FLG-END
           MOVE    "068"    TO        WRK-HKNNUM
           PERFORM  950-SEIKYU-SELECT-SEC
           PERFORM 2003-TOKUBETU-SYUKEI-SEC
                   UNTIL   FLG-END =   1
           PERFORM  950-SEIKYU-CLOSE-SEC
      *
           MOVE     ZERO    TO        FLG-END
           MOVE    "069"    TO        WRK-HKNNUM
           PERFORM  950-SEIKYU-SELECT-SEC
           PERFORM 2003-TOKUBETU-SYUKEI-SEC
                   UNTIL   FLG-END =   1
           PERFORM  950-SEIKYU-CLOSE-SEC
      * 
           MOVE     ZERO    TO        FLG-END
           MOVE    "040"    TO        WRK-HKNNUM
           PERFORM  950-SEIKYU-SELECT-SEC
           PERFORM  2003-TOKUBETU-SYUKEI-SEC
                    UNTIL   FLG-END   =   1
           PERFORM  950-SEIKYU-CLOSE-SEC
      *
           CLOSE   UPD-FILE
           CLOSE   SKT2000TMP-FILE
           CLOSE   SKT2001TMP-FILE
      *
           IF      FLG-UPD     =       1
               PERFORM 2002-PRINT-SEC
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
           DISPLAY "*** SOKATU2000 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU2000 END ***"
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
           INITIALIZE       SKT2000TMP-REC
      *
           IF  RECE10-HKNJANUM(7:2)  =  SPACE
             MOVE  RECE10-HKNJANUM(1:6)  TO  SKT2000TMP-HKNJANUM(1:6)
           ELSE
             MOVE  RECE10-HKNJANUM(3:6)  TO  SKT2000TMP-HKNJANUM(1:6)
           END-IF
      *
           MOVE    RECE10-PREFKBN        TO  SKT2000TMP-PREFKBN
           MOVE    "2"                   TO  SKT2000TMP-GOKEIKBN
           IF  RECE10-RJNHKNNUM  NOT =  SPACE
               MOVE  "1"                 TO  SKT2000TMP-RJNFLG
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
           PERFORM 2101-SYUKEI-SEC
      *
      *
      *    合計ファイル作成
           INITIALIZE     SKT2000TMP-REC
           MOVE    RECE10-PREFKBN           TO  SKT2000TMP-PREFKBN
           MOVE    "1"                      TO  SKT2000TMP-GOKEIKBN
      *
           PERFORM 900-SKTTMP-READ-SEC
      *
           PERFORM 2101-SYUKEI-SEC
      *
           MOVE    1    TO  FLG-UPD 
      *
           .
       210-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       2101-SYUKEI-SEC         SECTION.
      *
      *    療養の給付：件数
           ADD   1                 TO  SKT2000TMP-KENSU
      *
           IF  RECE10-ETC-KOHNUM(1)  =  "972" OR
               RECE10-ETC-KOHNUM(2)  =  "972" OR
               RECE10-ETC-KOHNUM(3)  =  "972" OR
               RECE10-ETC-KOHNUM(4)  =  "972"
             ADD   1                        TO  SKT2000TMP-TYOKI
           END-IF
      *
           IF  FLG-SKT2000TMP  =  1
               WRITE   SKT2000TMP-REC
           ELSE
               REWRITE SKT2000TMP-REC
           END-IF
           .
       2101-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    後期高齢集計処理
      *****************************************************************
       2002-KOUKI-SYUKEI-SEC              SECTION.
      * 
      *TMP-FILE キーセット
      *
           INITIALIZE       SKT2001TMP-REC
      *
           MOVE  RECE10-HKNJANUM(1:4)    TO  SKT2001TMP-HKNJANUM(1:4)
      *
           MOVE    RECE10-PREFKBN        TO  SKT2001TMP-PREFKBN
           MOVE    "2"                   TO  SKT2001TMP-GOKEIKBN
      *
           PERFORM  901-SKTTMP-READ-SEC
      *
           PERFORM  210-KOUKI-SYUKEI-SEC
      *
      *    集計の対象にならなかったとき
           IF      FLG-UPD             =   1
               PERFORM 230-UPD-FILE-HENSYU-SEC
      *
               PERFORM 950-SEIKYU-READ-SEC
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
       2002-KOUKI-SYUKEI-EXT.
           EXIT.
      *****************************************************************
      *    後期高齢集計処理
      *****************************************************************
       210-KOUKI-SYUKEI-SEC          SECTION.
      *
           PERFORM 2102-KOUKI-SYUKEI-SEC
      *
      *    合計ファイル作成
           INITIALIZE     SKT2001TMP-REC
           MOVE    RECE10-PREFKBN           TO  SKT2001TMP-PREFKBN
           MOVE    "1"                      TO  SKT2001TMP-GOKEIKBN
      *
           PERFORM 901-SKTTMP-READ-SEC
      *
           PERFORM 2102-KOUKI-SYUKEI-SEC
      *
           MOVE    1    TO  FLG-UPD 
      *
           .
       210-KOUKI-SYUKEI-EXT.
           EXIT.
      *****************************************************************
      *    集計処理
      *****************************************************************
       2102-KOUKI-SYUKEI-SEC         SECTION.
      *
      *    療養の給付：件数
           ADD   1                 TO  SKT2001TMP-KENSU
      *
           IF  RECE10-ETC-KOHNUM(1)  =  "972" OR
               RECE10-ETC-KOHNUM(2)  =  "972" OR
               RECE10-ETC-KOHNUM(3)  =  "972" OR
               RECE10-ETC-KOHNUM(4)  =  "972"
             ADD   1                        TO  SKT2001TMP-TYOKI
           END-IF
      *
           IF  FLG-SKT2001TMP  =  1
               WRITE   SKT2001TMP-REC
           ELSE
               REWRITE SKT2001TMP-REC
           END-IF
           .
       2102-KOUKI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    特別療養費国保集計処理
      *****************************************************************
       2003-TOKUBETU-SYUKEI-SEC              SECTION.
      *
           IF  RECE10-PREFKBN  =  "1"
              ADD   1      TO  WRK-TOKUKENSU(1)
           ELSE
              ADD   1      TO  WRK-TOKUKENSU(2)
           END-IF
      *
           MOVE    1       TO  FLG-UPD 
      *
      *    集計の対象にならなかったとき
           IF      FLG-UPD             =   1
               PERFORM 230-UPD-FILE-HENSYU-SEC
      *
               PERFORM 950-SEIKYU-READ-SEC
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
       2003-TOKUBETU-SYUKEI-EXT.
           EXIT.
      *
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
           OPEN     INPUT       SKT2000TMP-FILE
           OPEN     INPUT       SKT2001TMP-FILE
      *
           MOVE     ZERO        TO       FLG-PRINT
           MOVE     ZERO        TO       FLG-PRINT2
      *
      *    一時ファイル読込
           PERFORM 900-TMP-READ-SEC
           PERFORM 901-TMP-READ-SEC
      *
           PERFORM UNTIL   FLG-PRINT   =   1
                   AND     FLG-PRINT2  =   1
      *
      *        帳票編集<見出し>処理
               PERFORM 310-HEAD-HEN-SEC
      *
      *        帳票編集<明細>処理
               IF  FLG-PRINT  NOT =  1
                 PERFORM 320-BODY-HEN-SEC
               ELSE
                 MOVE  "2"  TO  KEY-OLD
               END-IF
      *
      *        帳票編集<明細>処理（後期高齢）
               IF  (KEY-OLD  =  1  AND  SKT2001TMP-PREFKBN  =  1)
               OR  (KEY-OLD  =  2)
                 PERFORM 321-BODY-HEN-SEC
               END-IF
      *
      *        帳票印刷処理
               PERFORM 330-PRINT-OUT-SEC
           END-PERFORM
      *
           CLOSE    SKT2000TMP-FILE
           CLOSE    SKT2001TMP-FILE
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
           ADD     1       TO  CNT-PAGE
      *
           INITIALIZE  SKT2000
           INITIALIZE  WRK-SKENSU
           INITIALIZE  WRK-STYOKI
           INITIALIZE  WRK-SKENSU2
      *
      *    診療年月
           IF   WRK-PARA-SYOKBN  =  "2"
           AND  FLG-RECEDEN  NOT =  ZERO
             MOVE  LNK-PRTKANRI-SRYYM        TO  WRK-SYMD (1:6)
             MOVE  "01"                      TO  WRK-SYMD (7:2)
             IF  WRK-SMM  =  1
               COMPUTE  WRK-SYY  =  WRK-SYY  -  1
               MOVE  12                      TO  WRK-SMM
             ELSE
               COMPUTE  WRK-SMM  =  WRK-SMM  -  1
             END-IF
             PERFORM 31012-SEIWA-HEN-SEC
             MOVE    WRK-HENYMDG(1:16)       TO  WRK-SRYYMWH2
             MOVE    WRK-SRYYMWH2            TO  SKT2000-SRYYM
           ELSE
             MOVE    WRK-SRYYMWH             TO  SKT2000-SRYYM
           END-IF
      *
      *    医療機関住所
           MOVE    WRK-HOSPADRS            TO  SKT2000-ADRS
           INSPECT SKT2000-ADRS            REPLACING ALL "  " BY "　"
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME            TO  SKT2000-HOSPNAME
           INSPECT SKT2000-HOSPNAME        REPLACING ALL "  " BY "　"
      *
      *    開設者名
           MOVE    WRK-KAISETUNAME         TO  SKT2000-KAISETUNAME
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD         TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SKT2000-HOSPCD
      *
      *    点数表区分
           EVALUATE    SYS-1001-TENHYOKBN
               WHEN    1   MOVE    "○"    TO  SKT2000-TENHYOKBN (1)
               WHEN    3   MOVE    "○"    TO  SKT2000-TENHYOKBN (2)
               WHEN    4   MOVE    "○"    TO  SKT2000-TENHYOKBN (3)
           END-EVALUATE
      *    標榜科名
           MOVE    WRK-SRYKANAME(1)        TO  SKT2000-SRYKA(1)
           MOVE    WRK-SRYKANAME(2)        TO  SKT2000-SRYKA(2)
           MOVE    WRK-SRYKANAME(3)        TO  SKT2000-SRYKA(3)
           MOVE    WRK-SRYKANAME(4)        TO  SKT2000-SRYKA(4)
           MOVE    WRK-SRYKANAME(5)        TO  SKT2000-SRYKA(5)
      *
      *    合計ファイルの場合、合計を印字する
           IF      SKT2000TMP-GOKEIKBN     =   "1"
               MOVE    SKT2000TMP-KENSU    TO  WRK-Z5
               MOVE    WRK-Z5              TO  SKT2000-GKENSU
               IF      SKT2000TMP-TYOKI    >   ZERO
                   MOVE    SKT2000TMP-TYOKI
                                          TO  WRK-Z6
                   STRING  "(" WRK-Z6 ")"     DELIMITED BY SIZE
                                      INTO    SKT2000-GTYOKI
                   END-STRING
               END-IF
               IF  SKT2000TMP-PREFKBN  =  "1"
                   MOVE   WRK-TOKUKENSU(1) TO  WRK-Z5
                   MOVE   WRK-Z5           TO  SKT2000-TOKUBETU
               ELSE
                   MOVE   WRK-TOKUKENSU(2) TO  WRK-Z5
                   MOVE   WRK-Z5           TO  SKT2000-TOKUBETU
               END-IF
      *
               PERFORM 900-TMP-READ-SEC
           END-IF
           IF      SKT2001TMP-GOKEIKBN     =   "1"
             IF  SKT2001TMP-PREFKBN  =  SKT2000TMP-PREFKBN
             OR  (SKT2000TMP-PREFKBN  NOT =  "1"  OR  "2")
               MOVE    SKT2001TMP-KENSU    TO  WRK-Z5
               MOVE    WRK-Z5              TO  SKT2000-KGKENSU
               IF  SKT2001TMP-PREFKBN  =  "1"
                   MOVE   WRK-TOKUKENSU(1) TO  WRK-Z5
                   MOVE   WRK-Z5           TO  SKT2000-TOKUBETU
               ELSE
                   MOVE   WRK-TOKUKENSU(2) TO  WRK-Z5
                   MOVE   WRK-Z5           TO  SKT2000-TOKUBETU
               END-IF
      *
               PERFORM 901-TMP-READ-SEC
             ELSE
               MOVE  SKT2001TMP-KENSU      TO  WRK-KENSU
               PERFORM 901-TMP-READ-SEC
             END-IF
           ELSE
             MOVE    WRK-KENSU           TO  WRK-Z5
             MOVE    WRK-Z5              TO  SKT2000-KGKENSU
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
           MOVE    KEY-NEW     TO  KEY-OLD
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       32
                   OR      FLG-PRINT       =   1
                   OR      KEY-NEW     NOT =   KEY-OLD
      *
      *        保険者番号
               MOVE    SKT2000TMP-HKNJANUM TO  WRK-HKNJANUM
               PERFORM 920-HKNJAINF-SET-SEC
               IF  SKT2000TMP-RJNFLG  =  1
                   STRING  HKNJA-HKNJANAME     DELIMITED BY SPACE
                           "（老健分）"        DELIMITED BY SIZE
                           INTO    SKT2000-HKNJANAME(IDX1)
                   END-STRING
               ELSE
                 MOVE  HKNJA-HKNJANAME  TO  SKT2000-HKNJANAME(IDX1)
               END-IF
      *
      *        療養の給付：件数
               MOVE    SKT2000TMP-KENSU    TO  WRK-Z5
               MOVE    WRK-Z5  TO  SKT2000-KENSU (IDX1)
      *
      *        長期
               IF      SKT2000TMP-TYOKI    >   ZERO
                   MOVE    SKT2000TMP-TYOKI    TO  WRK-Z5
                   STRING  "(" WRK-Z5 ")"      DELIMITED BY SIZE
                                       INTO    SKT2000-TYOKI(IDX1)
                   END-STRING
               END-IF
      *
      *        小計件数加算
               ADD     SKT2000TMP-TYOKI    TO  WRK-STYOKI
               ADD     SKT2000TMP-KENSU    TO  WRK-SKENSU
      *
      *        一時ファイル読込
               PERFORM 900-TMP-READ-SEC
           END-PERFORM
      *
      *    小計件数設定
           MOVE    WRK-SKENSU      TO  WRK-Z5
           MOVE    WRK-Z5          TO  SKT2000-SKENSU
      *
      *    小計長期件数設定
           IF      WRK-STYOKI      >   ZERO
               MOVE    WRK-STYOKI  TO  WRK-Z6
               STRING  "(" WRK-Z6 ")"  DELIMITED BY SIZE
                               INTO    SKT2000-STYOKI
               END-STRING
           END-IF
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       321-BODY-HEN-SEC      SECTION.
      *
      *    編集処理
           MOVE    KEY-NEW2    TO  KEY-OLD2
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       10
                   OR      FLG-PRINT2      =   1
                   OR      KEY-NEW2    NOT =   KEY-OLD2
      *
      *        広域連合名
               PERFORM  3210-KOUKINAME-SEC
               STRING  WRK-KENNAME    DELIMITED  BY  SPACE
                       "広域連合"     DELIMITED  BY  SIZE
                       INTO           SKT2000-KHKNJANAME(IDX1)
               END-STRING
      *
      *        療養の給付：件数
               MOVE    SKT2001TMP-KENSU    TO  WRK-Z5
               MOVE    WRK-Z5  TO  SKT2000-KKENSU (IDX1)
      *
      *        小計件数加算
               ADD     SKT2001TMP-KENSU    TO  WRK-SKENSU2
      *
      *        一時ファイル読込
               PERFORM 901-TMP-READ-SEC
           END-PERFORM
      *
      *    小計件数設定
           MOVE    WRK-SKENSU2     TO  WRK-Z5
           MOVE    WRK-Z5          TO  SKT2000-KSKENSU
      *
           .
       321-BODY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    後期高齢者広域連合名
      *****************************************************************
       3210-KOUKINAME-SEC      SECTION.
      *
           EVALUATE  SKT2001TMP-HKNJANUM (3:2)
           WHEN  "01"  MOVE  "北海道"    TO  WRK-KENNAME
           WHEN  "02"  MOVE  "青森県"    TO  WRK-KENNAME
           WHEN  "03"  MOVE  "岩手県"    TO  WRK-KENNAME
           WHEN  "04"  MOVE  "宮城県"    TO  WRK-KENNAME
           WHEN  "05"  MOVE  "秋田県"    TO  WRK-KENNAME
           WHEN  "06"  MOVE  "山形県"    TO  WRK-KENNAME
           WHEN  "07"  MOVE  "福島県"    TO  WRK-KENNAME
           WHEN  "08"  MOVE  "茨城県"    TO  WRK-KENNAME
           WHEN  "09"  MOVE  "栃木県"    TO  WRK-KENNAME
           WHEN  "10"  MOVE  "群馬県"    TO  WRK-KENNAME
           WHEN  "11"  MOVE  "埼玉県"    TO  WRK-KENNAME
           WHEN  "12"  MOVE  "千葉県"    TO  WRK-KENNAME
           WHEN  "13"  MOVE  "東京都"    TO  WRK-KENNAME
           WHEN  "14"  MOVE  "神奈川県"  TO  WRK-KENNAME
           WHEN  "15"  MOVE  "新潟県"    TO  WRK-KENNAME
           WHEN  "16"  MOVE  "富山県"    TO  WRK-KENNAME
           WHEN  "17"  MOVE  "石川県"    TO  WRK-KENNAME
           WHEN  "18"  MOVE  "福井県"    TO  WRK-KENNAME
           WHEN  "19"  MOVE  "山梨県"    TO  WRK-KENNAME
           WHEN  "20"  MOVE  "長野県"    TO  WRK-KENNAME
           WHEN  "21"  MOVE  "岐阜県"    TO  WRK-KENNAME
           WHEN  "22"  MOVE  "静岡県"    TO  WRK-KENNAME
           WHEN  "23"  MOVE  "愛知県"    TO  WRK-KENNAME
           WHEN  "24"  MOVE  "三重県"    TO  WRK-KENNAME
           WHEN  "25"  MOVE  "滋賀県"    TO  WRK-KENNAME
           WHEN  "26"  MOVE  "京都府"    TO  WRK-KENNAME
           WHEN  "27"  MOVE  "大阪府"    TO  WRK-KENNAME
           WHEN  "28"  MOVE  "兵庫県"    TO  WRK-KENNAME
           WHEN  "29"  MOVE  "奈良県"    TO  WRK-KENNAME
           WHEN  "30"  MOVE  "和歌山県"  TO  WRK-KENNAME
           WHEN  "31"  MOVE  "鳥取県"    TO  WRK-KENNAME
           WHEN  "32"  MOVE  "島根県"    TO  WRK-KENNAME
           WHEN  "33"  MOVE  "岡山県"    TO  WRK-KENNAME
           WHEN  "34"  MOVE  "広島県"    TO  WRK-KENNAME
           WHEN  "35"  MOVE  "山口県"    TO  WRK-KENNAME
           WHEN  "36"  MOVE  "徳島県"    TO  WRK-KENNAME
           WHEN  "37"  MOVE  "香川県"    TO  WRK-KENNAME
           WHEN  "38"  MOVE  "愛媛県"    TO  WRK-KENNAME
           WHEN  "39"  MOVE  "高知県"    TO  WRK-KENNAME
           WHEN  "40"  MOVE  "福岡県"    TO  WRK-KENNAME
           WHEN  "41"  MOVE  "佐賀県"    TO  WRK-KENNAME
           WHEN  "42"  MOVE  "長崎県"    TO  WRK-KENNAME
           WHEN  "43"  MOVE  "熊本県"    TO  WRK-KENNAME
           WHEN  "44"  MOVE  "大分県"    TO  WRK-KENNAME
           WHEN  "45"  MOVE  "宮崎県"    TO  WRK-KENNAME
           WHEN  "46"  MOVE  "鹿児島県"  TO  WRK-KENNAME
           WHEN  "47"  MOVE  "沖縄県"    TO  WRK-KENNAME
           END-EVALUATE
      *
           .
       3210-KOUKINAME-EXT.
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
           MOVE   "SKT2000.red"        TO  SPRT-PRTID
           EVALUATE  WRK-PARA-SYOKBN
             WHEN  "1"
               MOVE    "国保総括表"    TO  SPRT-TITLE
             WHEN  "2"
               MOVE    "国保総括表［返戻分］"
                                       TO  SPRT-TITLE
           END-EVALUATE
           MOVE    SKT2000             TO  SPRT-PRTDATA
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
                       MOVE  "*** SOKATU2000 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                   MOVE  "*** SOKATU2000 SEIKYU INVALID ***"
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
           MOVE    SPA-HOSPNUM          TO  HKNJA-HOSPNUM
           MOVE    WRK-HKNJANUM         TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
           MOVE    SPA-HOSPNUM              TO  RECE10-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM       TO  RECE10-SKYYM
      *
           MOVE    SPACE                    TO  RECE10-XXSTHKNJANUM
           MOVE    "99999999"               TO  RECE10-XXEDHKNJANUM
      *
           MOVE    RECE10-REC               TO  MCPDATA-REC
           MOVE    "DBSELECT"               TO  MCP-FUNC
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
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
               MOVE    1                   TO  FLG-END
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
           READ    SKT2000TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT2000TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT2000TMP
           END-READ
           .
       900-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計ファイル読込
      *****************************************************************
       901-SKTTMP-READ-SEC             SECTION.
      *
           READ    SKT2001TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT2001TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT2001TMP
           END-READ
           .
       901-SKTTMP-READ-EXT.
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
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  SKT2000TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
               NOT AT END
                   MOVE  SKT2000TMP-PREFKBN  TO  KEY-N-PREFKBN
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       901-TMP-READ-SEC                SECTION.
      *
           READ  SKT2001TMP-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT2
               NOT AT END
                   MOVE  SKT2001TMP-PREFKBN  TO  KEY-N-PREFKBN2
           END-READ
           .
       901-TMP-READ-EXT.
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
       900-DBCLOSE-EXT.
           EXIT.
