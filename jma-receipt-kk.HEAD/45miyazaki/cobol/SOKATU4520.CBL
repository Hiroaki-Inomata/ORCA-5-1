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
       PROGRAM-ID.             SOKATU4520.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次帳票
      *  コンポーネント名  : 国保・請求書集計処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/07/11    有村          再作成(2.5.0対応)
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev 	修正者	日付		内容
      *  01.00.01    内田    06/11/18    食事がない場合でも生活療養費を印字
      *  01.00.02    内田    07/10/06    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *  01.00.03    吉川    11/02/10    open-cobol1.0対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.

      *    請求書一時ファイル
           SELECT  SKT4520TMP-FILE ASSIGN    SKT4520TP
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  SKT4520TMP-KEY
                                   FILE    STATUS  IS  STS-SKT4520.
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
       FD  SKT4520TMP-FILE.
           COPY    "CPSKT4520TMP.INC".
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
                                   BY         //SKT4520TP//.
      *
      *
006900*    請求ＤＢ更新用ファイル 名称領域 
           COPY    "CPCOMMONDAT2.INC"
                                   REPLACING  //RECE01PARA//
                                   BY         //RECEUPD//.
           03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    エラーファイル 名称領域 
      *---(01.00.02) LINE ADD START ------------------------------------
      *     COPY    "CPCOMMONDAT2.INC"
      *                             REPLACING  //RECE01PARA//
      *                             BY         //RECEERR//.
      *     03  FILLER              PIC X(04)   VALUE   ".dat".
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *---(01.00.02) LINE ADD END   ------------------------------------
      *     
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
           COPY    "SKT4520.INC".
           COPY    "SKT4521.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-SKT4520         PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEUPD         PIC X(02).
           03  STS-YUSEN           PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SKT4520TMP      PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYSKANRI3       PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-UPD             PIC 9(01).
           03  FLG-UPDFILE         PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-PASS            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDX-MAX             PIC 9(04).
      *
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
      *---(01.00.02) LINE ADD START ------------------------------------
      *     03  WRK-PARA-HOSPID     PIC X(24).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *---(01.00.02) LINE ADD END   ------------------------------------
           03  WRK-PARA-HKNJANUM   PIC X(08).
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYOKBN     PIC X(01).
      *
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
           03  WRK-CNT             PIC  9(02).
           03  WRK-PAYKBN          PIC  9(03).
           03  WRK-RATE            PIC  9(03).
           03  WRK-HKNJANUM        PIC  X(06).
           03  WRK-KOH-KEY.
               05  WRK-KOHNUM      PIC  X(03).
               05  WRK-FTNRATE     PIC  9(01).
           03  WRK-KENNAME         PIC  X(10).
      *    ファイル名称領域
           03  FILE-NAME           PIC X(512).
      *
           03  WRK-Z1              PIC Z.
           03  WRK-Z2              PIC ZZ.
           03  WRK-Z4              PIC ZZZZ.
           03  WRK-Z5              PIC Z,ZZZ.
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
      *
           03  WRK-RECEERR         PIC X(200).
      *    文字変換
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
      *
      *---(01.00.02) LINE ADD START ------------------------------------
      *     03  WRK-PATH.
      *         05  FILLER          PIC S9(09)  BINARY.
      *         05  FILLER          PIC S9(09)  BINARY.
      *         05  FILLER          PIC S9(09)  BINARY.
           03  WRK-PATH            PIC X(50).
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
      *---(01.00.02) LINE ADD END   ------------------------------------
      *
      *公費ソート用エリア
       01  SORT-AREA.
           03  SORT-TBL.
               05  SORT-KOH-TBL  OCCURS  20.
                   07  SORT-KOHNUM-KEY.
                       09  SORT-KOHNUM          PIC  X(03).
                       09  SORT-KOH-FTNRATE     PIC  9(01).
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
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE            PIC 9(02).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-RECE10          PIC 9(06).
           03  CNT-UPDFILE         PIC 9(06).
           03  CNT-IN              PIC 9(02).
           03  CNT-KOH             PIC 9(02).
      *     
      *---(01.00.02) LINE ADD START ------------------------------------
      *     COPY    "ORCA-DBPATH".
           COPY    "COMMON-SPA".
      *---(01.00.02) LINE ADD END   ------------------------------------
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *　　保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先 
           COPY    "CPSK1002.INC".
      *
      *---(01.00.02) LINE ADD START ------------------------------------
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *---(01.00.02) LINE ADD END   ------------------------------------
      *
      *    請求管理情報
           COPY    "CPRCF010.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ファイルディレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
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
      *
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
      *
      *---(01.00.02) LINE ADD START ------------------------------------
      *                                         WRK-PARA-HOSPID
                                               WRK-PARA-HOSPNUM
      *---(01.00.02) LINE ADD END   ------------------------------------
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HKNJANUM
                                               WRK-PARA-SYOKBN
      *---(01.00.02) LINE ADD START ------------------------------------
                                               RECEERR
      *---(01.00.02) LINE ADD END   ------------------------------------
           END-UNSTRING
      *---(01.00.02) LINE ADD START ------------------------------------
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *---(01.00.02) LINE ADD END   ------------------------------------
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *---(01.00.02) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *---(01.00.02) LINE ADD END   ------------------------------------
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SOKATU4520"    TO  JOB-PGID
           MOVE    "県外・診療報酬請求書" TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *---(01.00.02) LINE ADD START ------------------------------------
                                   SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
      *
      *##（地方公費仕様：LNK-PRTKANRI-TERMID)
      *=== open-cobol1.0対応 by Yoshikawa(2011/02/10) start
      *     MOVE    "SKT4520TP"         TO  SKT4520TP-FILE-ID
           MOVE    "SKT4520"           TO  SKT4520TP-FILE-ID
      *=== open-cobol1.0対応 by Yoshikawa(2011/02/10) end
           MOVE    LNK-PRTKANRI-TERMID TO  SKT4520TP-TERMID
      *---(01.00.02) LINE ADD START ------------------------------------
           MOVE    WRK-PARA-HOSPNUM    TO  SKT4520TP-HOSPNUM
      *---(01.00.02) LINE ADD END   ------------------------------------
      *
           MOVE    "RECEUPD"           TO  RECEUPD-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  RECEUPD-TERMID
      *---(01.00.02) LINE ADD START ------------------------------------
           MOVE    WRK-PARA-HOSPNUM    TO  RECEUPD-HOSPNUM
      *---(01.00.02) LINE ADD END   ------------------------------------
      *
      *---(01.00.02) LINE ADD START ------------------------------------
      *     MOVE    "RECEERR"           TO  RECEERR-FILE-ID
      *     MOVE    LNK-PRTKANRI-TERMID TO  RECEERR-TERMID
      *---(01.00.02) LINE ADD END   ------------------------------------
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
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                       TO  SYS-1001-STYUKYMD(1:6)
                                           SYS-1001-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
                                           SYS-1001-EDYUKYMD(7:2)
      *---(01.00.02) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
      *---(01.00.02) LINE ADD END   ------------------------------------
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
           MOVE    LNK-PRTKANRI-SRYYM(1:6)
                                       TO  SYS-1002-STYUKYMD(1:6)
                                           SYS-1002-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1002-STYUKYMD(7:2)
                                           SYS-1002-EDYUKYMD(7:2)
      *---(01.00.02) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
      *---(01.00.02) LINE ADD END   ------------------------------------
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
      *---(01.00.02) LINE ADD START ------------------------------------
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *---(01.00.02) LINE ADD END   ------------------------------------
      *
           OPEN    OUTPUT  UPD-FILE
      *  
           EVALUATE    WRK-PARA-SYOKBN 
               WHEN    "2"
      *---(01.00.02) LINE ADD START ------------------------------------
      *             MOVE    PATH-TBL-SEIKYU-KEY37    TO  WRK-PATH
                   MOVE    "key37"                  TO  WRK-PATH
      *---(01.00.02) LINE ADD END   ------------------------------------
               WHEN    "1"
      *---(01.00.02) LINE ADD START ------------------------------------
      *             MOVE    PATH-TBL-SEIKYU-KEY38    TO  WRK-PATH
                   MOVE    "key38"                  TO  WRK-PATH
      *---(01.00.02) LINE ADD END   ------------------------------------
           END-EVALUATE
      *
           PERFORM 900-SEIKYU-SELECT-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           DISPLAY  "200-MAIN-SEC START"
      *
           OPEN    OUTPUT  SKT4520TMP-FILE
           CLOSE           SKT4520TMP-FILE
           OPEN    I-O     SKT4520TMP-FILE
      *
           MOVE    ZERO    TO  FLG-END
           MOVE    ZERO    TO  FLG-UPD
      *
           PERFORM 2001-KOKUHO-SYUKEI-SEC
                   UNTIL   FLG-END   =   1
      *
           CLOSE   UPD-FILE
           CLOSE   SKT4520TMP-FILE
      *
           IF      FLG-UPD   =       1
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
           DISPLAY "*** SOKATU4520 IN  "  CNT-RECE10 
           DISPLAY "*** SOKATU4520 END ***"
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *
      *---(01.00.02) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *---(01.00.02) LINE ADD END   ------------------------------------
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
      *---(01.00.02) LINE ADD START ------------------------------------
                                   SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------

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
           INITIALIZE       SKT4520TMP-REC
      *
           MOVE    RECE10-HKNJANUM-S       TO  SKT4520TMP-HKNJANUM
      *
      *    在総診・在医総チェック
           IF      RECE10-ZAITAKU  NOT =   SPACE
      *=== open-cobol1.0対応 by Yoshikawa(2011/02/10) start
      *         MOVE    1       TO  SKT4520TMP-ZAITAKU
               MOVE    "1"     TO  SKT4520TMP-ZAITAKU
      *=== open-cobol1.0対応 by Yoshikawa(2011/02/10) end
           END-IF
      *
      *
      *    国保組合の法定外給付判定
           IF      SKT4520TMP-HKNJANUM(3:1)    =   "3"
           AND     RECE10-RJNHKNNUM            =   SPACE
           AND     RECE10-AGE                  <   70
           AND     RECE10-AGE                  >   2
           AND     RECE10-KYURATE          NOT =   7
               MOVE    RECE10-KYURATE      TO  SKT4520TMP-KYURATE
           END-IF
      *
      *    TMPファイル読み込み
           PERFORM  800-SKTTMP-READ-SEC
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
      *    保険集計処理
      *****************************************************************
       210-HKN-SYUKEI-SEC          SECTION.
      *
      *    入院／外来
           IF      RECE10-NYUGAIKBN    =   "1"
               MOVE    1       TO   IDX2
           ELSE
               MOVE    2       TO   IDX2
           END-IF
      *
      *    老人かどうか
           IF      RECE10-RJNHKNNUM    NOT =   SPACE
               IF      RECE10-KYURATE  =   9
                   MOVE    10      TO  IDX1
               ELSE
                   MOVE    11      TO  IDX1
               END-IF
               MOVE    1   TO  SKT4520TMP-PAGE-FLG2
           ELSE
               MOVE    1   TO  SKT4520TMP-PAGE-FLG1
      *        一般国保
               IF      RECE10-HKNNUM   =   "060"
                   IF      RECE10-AGE  >=  70
                       IF      RECE10-KYURATE  =   9
                           MOVE    1       TO  IDX1
                       ELSE
                           MOVE    2       TO  IDX1
                       END-IF
                   ELSE
                       IF      RECE10-AGE  >=  3
                           MOVE    3       TO  IDX1
                       ELSE
                           MOVE    4       TO  IDX1
                       END-IF
                   END-IF
      *        退職国保
               ELSE
                   IF      RECE10-AGE  >=  70
                       IF      RECE10-KYURATE  =   9
                           MOVE    6       TO  IDX1
                       ELSE
                           MOVE    7       TO  IDX1
                       END-IF
                   ELSE
                       IF      RECE10-HONKZKKBN    =   "1"
                           MOVE    5       TO  IDX1
                       ELSE
                           IF      RECE10-AGE  <   3
                               MOVE    9       TO  IDX1
                           ELSE
                               MOVE    8       TO  IDX1
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    集計
           PERFORM 2101-SYUKEI-SEC
      *
      *    公費併用
           IF      RECE10-KOHNUM (1)   NOT =   SPACE
               MOVE    1       TO  SKT4520TMP-PAGE-FLG2
               PERFORM 2202-KOH-SEC
           END-IF
      *
      *    ２７老人併用時の原爆公費分集計処理
           IF      RECE10-ETC-KOHNUM(1)    =   "019"
           OR      RECE10-ETC-KOHNUM(2)    =   "019"
           OR      RECE10-ETC-KOHNUM(3)    =   "019"
           OR      RECE10-ETC-KOHNUM(4)    =   "019"
               PERFORM 22021-GENBK-KOH-SEC
           END-IF
      *
           IF      FLG-SKT4520TMP  =  1
               WRITE   SKT4520TMP-REC
           ELSE
               REWRITE SKT4520TMP-REC
           END-IF
      *
           MOVE    1               TO  FLG-UPD 
      *
           .
       210-HKN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療保険集計処理
      *****************************************************************
       2101-SYUKEI-SEC         SECTION.
      *
      *    療養の給付
      *    件数
           ADD     1                   TO  SKT4520TMP-KENSU(IDX1 IDX2)
      *
      *    日数
           ADD     RECE10-JNISSU(1)    TO  SKT4520TMP-NISSU(IDX1 IDX2)
      *
      *    点数
           ADD     RECE10-TOTALTEN(1)  TO  SKT4520TMP-TENSU(IDX1 IDX2)
      *
      *    一部負担金
           ADD     RECE10-FTNMONEY(1)  TO  SKT4520TMP-ITBFTN(IDX1 IDX2)
      *
      *    食事療養
           IF      RECE10-SHOKUJINISSU(1)  >   ZERO
      *        件数
               ADD     1               TO  SKT4520TMP-SKENSU(IDX1)
      *---(01.00.01) LINE ADD START ------------------------------------
           END-IF
      *---(01.00.01) LINE ADD END   ------------------------------------
      *    日数
           ADD     RECE10-SHOKUJINISSU(1)
                                       TO  SKT4520TMP-SNISSU(IDX1)
      *    金額
           ADD     RECE10-SHOKUJIRYOYOHI(1)
                                       TO  SKT4520TMP-SKINGK(IDX1)
      *    標準負担額
           ADD     RECE10-SHOKUJIFTN(1)
                                       TO  SKT4520TMP-SFTNGK(IDX1)
      *---(01.00.01) LINE ADD START ------------------------------------
      *     END-IF
      *---(01.00.01) LINE ADD END   ------------------------------------
           .
       2101-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    編集処理
      *****************************************************************
       2202-KOH-SEC              SECTION.
      *
      *    負担割合設定
           EVALUATE    RECE10-KYURATE
               WHEN    7
                   MOVE    3       TO  WRK-FTNRATE
               WHEN    8
                   MOVE    2       TO  WRK-FTNRATE
               WHEN    9
                   MOVE    1       TO  WRK-FTNRATE
               WHEN    OTHER
                   MOVE    ZERO    TO  WRK-FTNRATE
           END-EVALUATE
      *
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       4
                   OR      RECE10-KOHNUM(IDX3) =   SPACE
      *
      *       ０５１と０９１は同じ場所に集計する
               IF      RECE10-KOHNUM(IDX3) =   "091"
                   MOVE    "051"       TO  RECE10-KOHNUM(IDX3)
               END-IF
      *
      *
      *      公費・負担割合判別
               PERFORM  VARYING  IDX4  FROM  1  BY  1
                        UNTIL    IDX4  >     20
                        OR       SKT4520TMP-KOHNUM (IDX4) = SPACE
                   IF      RECE10-KOHNUM(IDX3)
                                       =   SKT4520TMP-KOHNUM(IDX4)
                   AND     WRK-FTNRATE =   SKT4520TMP-KOH-FTNRATE(IDX4)
                       MOVE    IDX4    TO  CNT-KOH
                       MOVE    20      TO  IDX4
                   END-IF
               END-PERFORM
      *
      *
      *       該当公費がなかった場合
               IF      IDX4    <   20
                   MOVE    IDX4    TO  CNT-KOH
               END-IF
      *
      *        負担割合
               MOVE    WRK-FTNRATE TO  SKT4520TMP-KOH-FTNRATE(CNT-KOH)
      *
      *        公費番号
               MOVE    RECE10-KOHNUM(IDX3)  
                               TO  SKT4520TMP-KOHNUM(CNT-KOH)
      *
      *        療養の給付：件数
               ADD     1       TO  SKT4520TMP-K-KENSU(CNT-KOH IDX2)
      *
      *        療養の給付:診療実日数
               ADD     RECE10-JNISSU(IDX3 + 1) 
                               TO  SKT4520TMP-K-NISSU(CNT-KOH IDX2)
      *
      *        療養の給付：点数
               ADD     RECE10-TOTALTEN(IDX3 + 1)
                               TO  SKT4520TMP-K-TENSU(CNT-KOH IDX2)
      *
      *        療養の給付：一部負担金
               ADD     RECE10-FTNMONEY (IDX3 + 1)
                               TO  SKT4520TMP-K-ITBFTN(CNT-KOH IDX2)
      *
      *        食事療養：件数
               IF      RECE10-SHOKUJINISSU (IDX3 + 1)  >   ZERO
                   ADD     1       TO  SKT4520TMP-K-SKENSU(CNT-KOH)
      *---(01.00.01) LINE ADD START ------------------------------------
               END-IF
      *---(01.00.01) LINE ADD END   ------------------------------------
      *        食事療養：日数
               ADD     RECE10-SHOKUJINISSU(IDX3 + 1)
                                   TO  SKT4520TMP-K-SNISSU(CNT-KOH)
      *        食事療養：金額
               ADD     RECE10-SHOKUJIRYOYOHI (IDX3 + 1)
                                   TO  SKT4520TMP-K-SKINGK(CNT-KOH)
      *        食事療養：標準負担額
               ADD     RECE10-SHOKUJIFTN (IDX3 + 1)
                                   TO  SKT4520TMP-K-SFTNGK(CNT-KOH)
      *---(01.00.01) LINE ADD START ------------------------------------
      *         END-IF
      *---(01.00.01) LINE ADD END   ------------------------------------
           END-PERFORM
      *
           .
       2202-KOH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ２７老人併用時の原爆公費分集計処理
      *****************************************************************
       22021-GENBK-KOH-SEC              SECTION.
      *
      *    負担割合設定
           EVALUATE    RECE10-KYURATE
               WHEN    7
                   MOVE    3       TO  WRK-FTNRATE
               WHEN    8
                   MOVE    2       TO  WRK-FTNRATE
               WHEN    9
                   MOVE    1       TO  WRK-FTNRATE
               WHEN    OTHER
                   MOVE    ZERO    TO  WRK-FTNRATE
           END-EVALUATE
      *
      *
      *    公費・負担割合判別
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL   IDX4    >       20
                   OR      SKT4520TMP-KOHNUM (IDX4) = SPACE
               IF      "019"       =   SKT4520TMP-KOHNUM(IDX4)
               AND     WRK-FTNRATE =   SKT4520TMP-KOH-FTNRATE(IDX4)
                   MOVE    IDX4    TO  CNT-KOH
                   MOVE    20      TO  IDX4
               END-IF
           END-PERFORM
      *
      *    該当公費がなかった場合
           IF      IDX4    <   20
               MOVE    IDX4    TO  CNT-KOH
           END-IF
      *
      *    負担割合
           MOVE    WRK-FTNRATE TO  SKT4520TMP-KOH-FTNRATE(CNT-KOH)
      *
      *    公費番号
           MOVE    "019"
                               TO  SKT4520TMP-KOHNUM(CNT-KOH)
      *
      *    療養の給付：件数
           ADD     1           TO  SKT4520TMP-K-KENSU(CNT-KOH IDX2)
      *
      *    療養の給付:診療実日数
           ADD     RECE10-JNISSU(1) 
                               TO  SKT4520TMP-K-NISSU(CNT-KOH IDX2)
      *
      *    療養の給付：点数
           ADD     RECE10-TOTALTEN(1)
                               TO  SKT4520TMP-K-TENSU(CNT-KOH IDX2)
           IF      RECE10-SHOKUJINISSU (1)  >  ZERO
      *        食事療養：件数
               ADD  1          TO  SKT4520TMP-K-SKENSU(CNT-KOH)
      *---(01.00.01) LINE ADD START ------------------------------------
           END-IF
      *---(01.00.01) LINE ADD END   ------------------------------------
      *    食事療養：日数
           ADD  RECE10-SHOKUJINISSU(1)
                               TO  SKT4520TMP-K-SNISSU(CNT-KOH)
      *    食事療養：金額
           ADD  RECE10-SHOKUJIRYOYOHI (1)
                               TO  SKT4520TMP-K-SKINGK(CNT-KOH)
      *    食事療養：標準負担額
           ADD  RECE10-SHOKUJIFTN (1)
                               TO  SKT4520TMP-K-SFTNGK(CNT-KOH)
      *---(01.00.01) LINE ADD START ------------------------------------
      *     END-IF
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
           .
       22021-GENBK-KOH-EXT.
           EXIT.
      *
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
           OPEN    INPUT   SKT4520TMP-FILE
      *
           MOVE    ZERO    TO  FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM UNTIL   FLG-PRINT   =   1
               MOVE    ZERO    TO  FLG-PASS
               MOVE    1       TO  WRK-CNT
      *
               IF      SKT4520TMP-PAGE-FLG1    =   1
      *
      *            帳票編集<見出し>処理
                   PERFORM 310-HEAD-HEN-SEC
      *
      *            帳票編集<明細>処理
                   PERFORM 320-BODY-HEN-SEC
      *
      *            帳票印刷処理
                   PERFORM 330-PRINT-OUT-SEC
               END-IF
      *
      *        公費は残っているか
               IF     SKT4520TMP-PAGE-FLG2  =  1
      *            公費ソート処理
                   PERFORM KOH-SORT-SEC
                   PERFORM UNTIL   ((SKT4520TMP-KOHNUM(WRK-CNT) = SPACE)
                           AND      (FLG-PASS  =  1))
                           OR      (WRK-CNT     >   20)
      *
      *                帳票編集<見出し>処理
                       PERFORM 311-HEAD-HEN-SEC
      *
      *                帳票編集<公費>処理
                       PERFORM 321-BODY-HEN-SEC
      *
      *                帳票印刷処理
                       PERFORM 331-PRINT-OUT-SEC
                       MOVE    1       TO  FLG-PASS
                   END-PERFORM
               END-IF
      *
      *        一時ファイル読込
               PERFORM  900-TMP-READ-SEC
           END-PERFORM
      *
           CLOSE    SKT4520TMP-FILE
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
           INITIALIZE  SKT4520
      *
      *    NO
           MOVE    CNT-PAGE                TO  WRK-Z4
           MOVE    WRK-Z4                  TO  SKT4520-NO
      *
      *    診療年月
           MOVE    WRK-SRYYMWH             TO  SKT4520-SRYYM
      *
      *    請求年月日
           MOVE    WRK-SEIYMDWH            TO  SKT4520-SEIYMD
      *
      *---(01.00.02) LINE ADD START ------------------------------------
      *    医療機関住所
      *     MOVE    SYS-1002-ADRS           TO  SKT4520-ADRS
      *
      *    医療機関名
      *     MOVE    SYS-1001-HOSPNAME       TO  SKT4520-HOSPNAME
      *    医療機関住所
           MOVE    WRK-HOSPADRS            TO  SKT4520-ADRS
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME            TO  SKT4520-HOSPNAME
      *---(01.00.02) LINE ADD END   ------------------------------------
      *
      *    開設者名
           MOVE    SYS-1001-KAISETUNAME    TO  SKT4520-KAISETU
      *
      *---(01.00.02) LINE ADD START ------------------------------------
      *    電話番号
      *     MOVE    SYS-1002-TEL            TO  SKT4520-TEL
      *    電話番号
           MOVE    WRK-HOSPTEL             TO  SKT4520-TEL
      *---(01.00.02) LINE ADD END   ------------------------------------
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD         TO  WRK-MAE-INPUT
      *
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SKT4520-HOSPCD
      *
      *    保険者番号
           MOVE    SKT4520TMP-HKNJANUM     TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SKT4520-HKNJANUM
      *
      *    保険者名
           MOVE    SKT4520TMP-HKNJANUM     TO  WRK-HKNJANUM
           PERFORM 920-HKNJAINF-SET-SEC
           IF      HKNJA-HKNJANAME      NOT  =    SPACE
               IF  SKT4520TMP-HKNJANUM(3:1) NOT  =  "3"
      *
      *            県名編集
                   PERFORM 3101-KENNAME-HEN-SEC
                   STRING  WRK-KENNAME         DELIMITED BY SIZE
                           HKNJA-HKNJANAME     DELIMITED BY SPACE
                           "  殿"              DELIMITED BY SIZE
                           INTO                SKT4520-HKNJANAME
                   END-STRING
               ELSE
      *
      *            国保組合は県名と"市町村"を記載しない
                   STRING  HKNJA-HKNJANAME     DELIMITED BY SPACE
                           "  殿"              DELIMITED BY SIZE
                           INTO                SKT4520-HKNJANAME
                   END-STRING
               END-IF
           ELSE
               MOVE    "  殿"      TO  SKT4520-HKNJANAME(25:4)
           END-IF
      *
      *    在宅
           IF      SKT4520TMP-ZAITAKU  =   "1"
               MOVE    "在"        TO  SKT4520-ZAI1
               MOVE    "○"        TO  SKT4520-ZAI2
           END-IF
      *
      *    給付割合
           MOVE    SKT4520TMP-KYURATE  TO  WRK-Z2
           MOVE    WRK-Z2              TO  SKT4520-KYURATE
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    県名編集処理
      *****************************************************************
       3101-KENNAME-HEN-SEC                SECTION.
      *
           INITIALIZE  WRK-KENNAME
           EVALUATE    SKT4520TMP-HKNJANUM (1:2)
               WHEN    "01"    MOVE    "北海道"    TO  WRK-KENNAME
               WHEN    "02"    MOVE    "青森県"    TO  WRK-KENNAME
               WHEN    "03"    MOVE    "岩手県"    TO  WRK-KENNAME
               WHEN    "04"    MOVE    "宮城県"    TO  WRK-KENNAME
               WHEN    "05"    MOVE    "秋田県"    TO  WRK-KENNAME
               WHEN    "06"    MOVE    "山形県"    TO  WRK-KENNAME
               WHEN    "07"    MOVE    "福島県"    TO  WRK-KENNAME
               WHEN    "08"    MOVE    "茨城県"    TO  WRK-KENNAME
               WHEN    "09"    MOVE    "栃木県"    TO  WRK-KENNAME
               WHEN    "10"    MOVE    "群馬県"    TO  WRK-KENNAME
               WHEN    "11"    MOVE    "埼玉県"    TO  WRK-KENNAME
               WHEN    "12"    MOVE    "千葉県"    TO  WRK-KENNAME
               WHEN    "13"    MOVE    "東京都"    TO  WRK-KENNAME
               WHEN    "14"    MOVE    "神奈川県"  TO  WRK-KENNAME
               WHEN    "15"    MOVE    "新潟県"    TO  WRK-KENNAME
               WHEN    "16"    MOVE    "富山県"    TO  WRK-KENNAME
               WHEN    "17"    MOVE    "石川県"    TO  WRK-KENNAME
               WHEN    "18"    MOVE    "福井県"    TO  WRK-KENNAME
               WHEN    "19"    MOVE    "山梨県"    TO  WRK-KENNAME
               WHEN    "20"    MOVE    "長野県"    TO  WRK-KENNAME
               WHEN    "21"    MOVE    "岐阜県"    TO  WRK-KENNAME
               WHEN    "22"    MOVE    "静岡県"    TO  WRK-KENNAME
               WHEN    "23"    MOVE    "愛知県"    TO  WRK-KENNAME
               WHEN    "24"    MOVE    "三重県"    TO  WRK-KENNAME
               WHEN    "25"    MOVE    "滋賀県"    TO  WRK-KENNAME
               WHEN    "26"    MOVE    "京都府"    TO  WRK-KENNAME
               WHEN    "27"    MOVE    "大阪府"    TO  WRK-KENNAME
               WHEN    "28"    MOVE    "兵庫県"    TO  WRK-KENNAME
               WHEN    "29"    MOVE    "奈良県"    TO  WRK-KENNAME
               WHEN    "30"    MOVE    "和歌山県"  TO  WRK-KENNAME
               WHEN    "31"    MOVE    "鳥取県"    TO  WRK-KENNAME
               WHEN    "32"    MOVE    "島根県"    TO  WRK-KENNAME
               WHEN    "33"    MOVE    "岡山県"    TO  WRK-KENNAME
               WHEN    "34"    MOVE    "広島県"    TO  WRK-KENNAME
               WHEN    "35"    MOVE    "山口県"    TO  WRK-KENNAME
               WHEN    "36"    MOVE    "徳島県"    TO  WRK-KENNAME
               WHEN    "37"    MOVE    "香川県"    TO  WRK-KENNAME
               WHEN    "38"    MOVE    "愛媛県"    TO  WRK-KENNAME
               WHEN    "39"    MOVE    "高知県"    TO  WRK-KENNAME
               WHEN    "40"    MOVE    "福岡県"    TO  WRK-KENNAME
               WHEN    "41"    MOVE    "佐賀県"    TO  WRK-KENNAME
               WHEN    "42"    MOVE    "長崎県"    TO  WRK-KENNAME
               WHEN    "43"    MOVE    "熊本県"    TO  WRK-KENNAME
               WHEN    "44"    MOVE    "大分県"    TO  WRK-KENNAME
               WHEN    "45"    MOVE    "宮崎県"    TO  WRK-KENNAME
               WHEN    "46"    MOVE    "鹿児島県"  TO  WRK-KENNAME
               WHEN    "47"    MOVE    "沖縄県"    TO  WRK-KENNAME
           END-EVALUATE
      *
           .
       3101-KENNAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC      SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       9
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       2
      *
      *            療養の給付：件数
                   MOVE    SKT4520TMP-KENSU(IDX1 IDX2) TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT4520-KENSU(IDX1 IDX2)
      *
      *            療養の給付：日数
                   MOVE    SKT4520TMP-NISSU(IDX1 IDX2) TO  WRK-Z5
                   MOVE    WRK-Z5      TO  SKT4520-NISSU(IDX1 IDX2)
      *
      *            療養の給付：点数
                   MOVE    SKT4520TMP-TENSU(IDX1 IDX2) TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT4520-TENSU(IDX1 IDX2)
      *
      *           療養の給付：一部負担金
                   MOVE    SKT4520TMP-ITBFTN(IDX1 IDX2)    TO  WRK-Z9
                   MOVE    WRK-Z9      TO  SKT4520-ITBFTN(IDX1 IDX2)
               END-PERFORM
      *
      *        食事療養：件数
               MOVE    SKT4520TMP-SKENSU(IDX1)     TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT4520-SKENSU(IDX1)
      *
      *        食事療養：日数
               MOVE    SKT4520TMP-SNISSU(IDX1)     TO  WRK-Z5
               MOVE    WRK-Z5      TO  SKT4520-SNISSU(IDX1)
      *
      *        食事療養：金額
               MOVE    SKT4520TMP-SKINGK(IDX1)     TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT4520-SKINGK(IDX1)
      *
      *        食事療養：標準負担額
               MOVE    SKT4520TMP-SFTNGK(IDX1)     TO  WRK-Z9
               MOVE    WRK-Z9      TO  SKT4520-SFTNGK(IDX1)
           END-PERFORM
      *
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
           MOVE   "SKT4520.red"        TO  SPRT-PRTID
           MOVE    "県外・国保請求書"  TO  SPRT-TITLE
           MOVE    SKT4520             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      *---(01.00.02) LINE ADD START ------------------------------------
                                       SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
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
      *    帳票編集<ヘッダー部>処理（公費）
      *****************************************************************
       311-HEAD-HEN-SEC          SECTION.
      *
      *
           ADD     1       TO  CNT-PAGE
      *
           INITIALIZE  SKT4521
      *
      *    NO
           MOVE    CNT-PAGE                TO  WRK-Z4
           MOVE    WRK-Z4                  TO  SKT4521-NO
      *
      *    診療年月
           MOVE    WRK-SRYYMWH             TO  SKT4521-SRYYM
      *
      *    請求年月日
           MOVE    WRK-SEIYMDWH            TO  SKT4521-SEIYMD
      *
      *---(01.00.02) LINE ADD START ------------------------------------
      *    医療機関住所
      *     MOVE    SYS-1002-ADRS           TO  SKT4521-ADRS
      *
      *    医療機関名
      *     MOVE    SYS-1001-HOSPNAME       TO  SKT4521-HOSPNAME
      *    医療機関住所
           MOVE    WRK-HOSPADRS            TO  SKT4521-ADRS
      *
      *    医療機関名
           MOVE    WRK-HOSPNAME            TO  SKT4521-HOSPNAME
      *---(01.00.02) LINE ADD END   ------------------------------------
      *
      *    開設者名
           MOVE    SYS-1001-KAISETUNAME    TO  SKT4521-KAISETU
      *
      *---(01.00.02) LINE ADD START ------------------------------------
      *    電話番号
      *     MOVE    SYS-1002-TEL            TO  SKT4521-TEL
      *    電話番号
           MOVE    WRK-HOSPTEL             TO  SKT4521-TEL
      *---(01.00.02) LINE ADD END   ------------------------------------
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD         TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SKT4521-HOSPCD
      *
      *    保険者番号
           MOVE    SKT4520TMP-HKNJANUM     TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT           TO  SKT4521-HKNJANUM
      *
      *    保険者名
           MOVE    SKT4520TMP-HKNJANUM     TO  WRK-HKNJANUM
           PERFORM 920-HKNJAINF-SET-SEC
           IF      HKNJA-HKNJANAME     NOT =   SPACE
               IF      SKT4520TMP-HKNJANUM(3:1)    NOT =   "3"
      *
      *            県名編集
                   PERFORM 3101-KENNAME-HEN-SEC
                   STRING  WRK-KENNAME         DELIMITED BY SIZE
                           HKNJA-HKNJANAME     DELIMITED BY SPACE
                           "  殿"              DELIMITED BY SIZE
                           INTO                SKT4521-HKNJANAME
                   END-STRING
               ELSE
      *
      *          国保組合は県名と"市町村"を記載しない
                   STRING  HKNJA-HKNJANAME     DELIMITED BY SPACE
                           "  殿"              DELIMITED BY SIZE
                           INTO                SKT4521-HKNJANAME
                   END-STRING
               END-IF
           ELSE
               MOVE    "  殿"      TO  SKT4521-HKNJANAME(25:4)
           END-IF
      *
      *    在宅
           IF      SKT4520TMP-ZAITAKU  =   "1"
               MOVE    "在"        TO  SKT4521-ZAI1
               MOVE    "○"        TO  SKT4521-ZAI2
           END-IF
      *
           .
       311-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理（公費）
      *****************************************************************
       321-BODY-HEN-SEC      SECTION.
      *
      *    公費の為の２枚目以上か
           IF      WRK-CNT     =   1
               PERFORM VARYING IDX1    FROM    10  BY  1
                       UNTIL   IDX1    >       11
                   COMPUTE IDX4    =   IDX1    -   9
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL   IDX2    >       2
      *
      *                療養の給付：件数
                       MOVE    SKT4520TMP-KENSU(IDX1 IDX2) TO  WRK-Z5
                       MOVE    WRK-Z5  TO  SKT4521-KENSU(IDX4 IDX2)
      *
      *                療養の給付：日数
                       MOVE    SKT4520TMP-NISSU(IDX1 IDX2) TO  WRK-Z5
                       MOVE    WRK-Z5  TO  SKT4521-NISSU(IDX4 IDX2)
      *
      *                療養の給付：点数
                       MOVE    SKT4520TMP-TENSU(IDX1 IDX2) TO  WRK-Z9
                       MOVE    WRK-Z9  TO  SKT4521-TENSU(IDX4 IDX2)
      *
      *                療養の給付：一部負担金
                       MOVE    SKT4520TMP-ITBFTN(IDX1 IDX2)
                                                           TO  WRK-Z9
                       MOVE    WRK-Z9  TO  SKT4521-ITBFTN(IDX4 IDX2)
                   END-PERFORM
      *
      *            食事療養：件数
                   MOVE    SKT4520TMP-SKENSU(IDX1)     TO  WRK-Z5
                   MOVE    WRK-Z5  TO  SKT4521-SKENSU(IDX4)
      *
      *            食事療養：日数
                   MOVE    SKT4520TMP-SNISSU(IDX1)     TO  WRK-Z5
                   MOVE    WRK-Z5  TO  SKT4521-SNISSU(IDX4)
      *
      *            食事療養：金額
                   MOVE    SKT4520TMP-SKINGK(IDX1)     TO  WRK-Z9
                   MOVE    WRK-Z9  TO  SKT4521-SKINGK(IDX4)
      *
      *            食事療養：標準負担額
                   MOVE    SKT4520TMP-SFTNGK(IDX1)     TO  WRK-Z9
                   MOVE    WRK-Z9  TO  SKT4521-SFTNGK(IDX4)
               END-PERFORM
           END-IF
      *
      *
      *    公費の印字
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       5
                   OR      WRK-CNT >       20
                   OR      SKT4520TMP-KOHNUM(WRK-CNT)  =   SPACE
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       2
      *
      *            療養の給付：件数
                   MOVE    SKT4520TMP-K-KENSU(WRK-CNT IDX2)  TO  WRK-Z5
                   MOVE    WRK-Z5  TO  SKT4521-K-KENSU(IDX3 IDX2)
      *
      *            療養の給付：日数
                   MOVE    SKT4520TMP-K-NISSU(WRK-CNT IDX2)  TO  WRK-Z5
                   MOVE    WRK-Z5  TO  SKT4521-K-NISSU(IDX3 IDX2)
      *
      *            療養の給付：点数
                   MOVE    SKT4520TMP-K-TENSU(WRK-CNT IDX2)  TO  WRK-Z9
                   MOVE    WRK-Z9  TO  SKT4521-K-TENSU(IDX3 IDX2)
      *
      *            療養の給付：一部負担金
                   MOVE    SKT4520TMP-K-ITBFTN(WRK-CNT IDX2) TO  WRK-Z9
                   MOVE    WRK-Z9  TO  SKT4521-K-ITBFTN(IDX3 IDX2)
               END-PERFORM
      *
      *        食事療養：件数
               MOVE    SKT4520TMP-K-SKENSU(WRK-CNT)    TO  WRK-Z5
               MOVE    WRK-Z5  TO  SKT4521-K-SKENSU(IDX3)
      *
      *        食事療養：日数
               MOVE    SKT4520TMP-K-SNISSU(WRK-CNT)    TO  WRK-Z5
               MOVE    WRK-Z5  TO  SKT4521-K-SNISSU(IDX3)
      *
      *        食事療養：金額
               MOVE    SKT4520TMP-K-SKINGK(WRK-CNT)    TO  WRK-Z9
               MOVE    WRK-Z9  TO  SKT4521-K-SKINGK(IDX3)
      *
      *        食事療養：標準負担額
               MOVE    SKT4520TMP-K-SFTNGK(WRK-CNT)    TO  WRK-Z9
               MOVE    WRK-Z9  TO  SKT4521-K-SFTNGK(IDX3)
      *
      *        法別番号
               IF      SKT4520TMP-KOHNUM (WRK-CNT) =   "019"
                   MOVE    "43"    TO  SKT4521-HBTNUM(IDX3)
               ELSE
                   MOVE    SKT4520TMP-KOHNUM (WRK-CNT) TO  WRK-KOHNUM
                   PERFORM 950-HKNNUM-SET-SEC
                   MOVE    HKN-HBTNUM      TO  SKT4521-HBTNUM(IDX3)
               END-IF
      *
      *        負担割合
               MOVE  SKT4520TMP-KOH-FTNRATE(WRK-CNT)
                                      TO  SKT4521-FTNRATE(IDX3)
      *
               ADD  1  TO  WRK-CNT
           END-PERFORM
      *
           .
       321-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理（公費）
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
           MOVE   "SKT4521.red"        TO  SPRT-PRTID
           MOVE    "県外・国保請求書"  TO  SPRT-TITLE
           MOVE    SKT4521             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      *---(01.00.02) LINE ADD START ------------------------------------
                                       SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
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
      *     MOVE    SPACE   TO    WRK-KOHNUM
           INITIALIZE  WRK-KOH-KEY
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                    UNTIL    IDX1  >  20
             IF  SKT4520TMP-KOHNUM(IDX1)  =  SPACE
               MOVE  20    TO    IDX1
             ELSE
               IF    WRK-KOHNUM                =   SPACE   OR
                     SKT4520TMP-KOH-KEY(IDX1)  <   WRK-KOH-KEY
                 MOVE     IDX1                TO  IDX2
                 MOVE     SKT4520TMP-KOH-KEY(IDX1) TO  WRK-KOH-KEY
               END-IF
               ADD   1     TO    IDX-MAX
             END-IF
           END-PERFORM
           IF     IDX2     >     ZERO
             MOVE    SKT4520TMP-KOH-TBL(IDX2) TO  SORT-KOH-TBL(1)
           END-IF
      
           DISPLAY "一般ソート前１" SKT4520TMP-KOH-KEY(1)
           DISPLAY "一般ソート前２" SKT4520TMP-KOH-KEY(2)
           DISPLAY "一般ソート前３" SKT4520TMP-KOH-KEY(3)
      *
           IF      IDX-MAX    >     ZERO
             MOVE    2    TO    IDX1
             MOVE    1    TO    IDX2
             MOVE    1    TO    IDX3
      *=== open-cobol1.0対応 by Yoshikawa(2011/02/10) start
      *       MOVE   9999  TO    WRK-KOH-KEY
             MOVE   HIGH-VALUE  TO    WRK-KOH-KEY
      *=== open-cobol1.0対応 by Yoshikawa(2011/02/10) end
             PERFORM    UNTIL    IDX1  >    IDX-MAX
               PERFORM  VARYING  IDX2  FROM  1  BY  1
                        UNTIL    IDX2  >    IDX-MAX
                 IF  SKT4520TMP-KOH-KEY(IDX2)
                                       > SORT-KOHNUM-KEY(IDX1 - 1)
                 AND SKT4520TMP-KOH-KEY(IDX2)
                                       < WRK-KOH-KEY
                     MOVE  SKT4520TMP-KOH-KEY(IDX2)  TO WRK-KOH-KEY
                     MOVE  IDX2          TO    IDX3
                 END-IF
               END-PERFORM
               MOVE  SKT4520TMP-KOH-TBL(IDX3)  TO  SORT-KOH-TBL(IDX1)
      *=== open-cobol1.0対応 by Yoshikawa(2011/02/10) start
      *         MOVE  9999 TO    WRK-KOH-KEY
               MOVE  HIGH-VALUE  TO    WRK-KOH-KEY
      *=== open-cobol1.0対応 by Yoshikawa(2011/02/10) end
               ADD   1    TO    IDX1
             END-PERFORM
           END-IF
      *
           INITIALIZE             SKT4520TMP-KOH-AREA
           MOVE  SORT-AREA  TO    SKT4520TMP-KOH-AREA
      *
           DISPLAY "一般ソート後１" SKT4520TMP-KOH-KEY(1)
           DISPLAY "一般ソート後２" SKT4520TMP-KOH-KEY(2)
           DISPLAY "一般ソート後３" SKT4520TMP-KOH-KEY(3)
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
      *---(01.00.02) LINE ADD START ------------------------------------
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *---(01.00.02) LINE ADD END   ------------------------------------
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC
      *---(01.00.02) LINE ADD START ------------------------------------
                                       SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
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
      *
                   MOVE    LNK-PRTKANRI-SRYYM
                                           TO  RECE10-SKYYM
                   MOVE    LNK-PRTKANRI-SKYYMD
                                           TO  RECE10-UPDYMD
      *
                   MOVE    RECE10-REC      TO  MCPDATA-REC
                   MOVE    "DBUPDATE"      TO  MCP-FUNC
      *---(01.00.02) LINE ADD START ------------------------------------
      *             MOVE    PATH-TBL-SEIKYU-KEY
      *                                     TO  MCP-PATH
      *             CALL    "MCPSUB"        USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
                   MOVE    "tbl_seikyu"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"     USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
                   IF      MCP-RC          =   ZERO
                       PERFORM 900-UPD-FILE-READ-SEC
                   ELSE
                       DISPLAY "UPD ERR KEY="  RECE10-KEY 
                       MOVE  "*** SOKATU4520 SEIKYU UPD ERR ***"
                                           TO  WRK-RECEERR
                       MOVE    RECE10-KEY  TO  WRK-RECEERR(33:)
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1           TO  FLG-UPDFILE
                   END-IF
               ELSE
                       DISPLAY "INV ERR KEY="  RECE10-KEY 
                   MOVE  "*** SOKATU4520 SEIKYU INVALID ***"
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
      *---(01.00.02) LINE ADD START ------------------------------------
      *     MOVE    SYS-1001-HOSPID         TO  HKNJA-HOSPID
           MOVE    SPA-HOSPNUM             TO  HKNJA-HOSPNUM
      *---(01.00.02) LINE ADD END   ------------------------------------
           MOVE    WRK-HKNJANUM            TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
      *---(01.00.02) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-HKNJAINF-KEY   TO  MCP-PATH
      *     CALL    "MCPSUB"                USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
           MOVE  "tbl_hknjainf"           TO  MCP-TABLE
           MOVE  "key"                    TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"              USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
           IF  MCP-RC  =  ZERO
      *---(01.00.02) LINE ADD START ------------------------------------
      *         MOVE  PATH-TBL-HKNJAINF-KEY       TO  MCP-PATH
               MOVE  "tbl_hknjainf"           TO  MCP-TABLE
               MOVE  "key"                    TO  MCP-PATHNAME
      *---(01.00.02) LINE ADD END   ------------------------------------
               PERFORM  925-HKNJAINF-READ-SEC
           ELSE
               DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
      *---(01.00.02) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-HKNJAINF-KEY   TO  MCP-PATH
      *     CALL    "MCPSUB"                USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
           MOVE  "tbl_hknjainf"           TO  MCP-TABLE
           MOVE  "key"                    TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"              USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
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
      *---(01.00.02) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"            USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
           CALL  "ORCDBMAIN"              USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
           IF      MCP-RC  =   ZERO
               MOVE    MCPDATA-REC TO  HKNJAINF-REC
               MOVE    ZERO        TO  FLG-HKNJAINF
           ELSE
               INITIALIZE  HKNJAINF-REC
               MOVE    1           TO  FLG-HKNJAINF
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
      *---(01.00.02) LINE ADD START ------------------------------------
      *     MOVE    SYS-1001-HOSPID      TO  HKN-HOSPID
           MOVE    SPA-HOSPNUM          TO  HKN-HOSPNUM
      *---(01.00.02) LINE ADD END   ------------------------------------
           MOVE    WRK-KOHNUM           TO  HKN-HKNNUM
           MOVE    HKNNUM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
      *---(01.00.02) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-HKNNUM-KEY2     TO  MCP-PATH
      *     CALL    "MCPSUB"                 USING
      *                                          MCPAREA
      *                                          MCPDATA-REC
           MOVE  "tbl_hknnum"          TO  MCP-TABLE
           MOVE  "key2"                TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
           IF  MCP-RC  =  ZERO
      *---(01.00.02) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-HKNNUM-KEY2    TO  MCP-PATH
               MOVE  "tbl_hknnum"        TO  MCP-TABLE
               MOVE  "key2"              TO  MCP-PATHNAME
      *---(01.00.02) LINE ADD END   ------------------------------------
               PERFORM  955-HKNNUM-READ-SEC
           ELSE
               DISPLAY  "HKNNUM SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE    "DBCLOSECURSOR"             TO  MCP-FUNC
      *---(01.00.02) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-HKNNUM-KEY2        TO  MCP-PATH
      *     CALL    "MCPSUB"                    USING
      *                                             MCPAREA
      *                                             MCPDATA-REC
           MOVE  "tbl_hknnum"          TO  MCP-TABLE
           MOVE  "key2"                TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
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
      *---(01.00.02) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"            USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
           CALL  "ORCDBMAIN"        USING
                                    MCPAREA
                                    MCPDATA-REC
                                    SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
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
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
      *
      *---(01.00.02) LINE ADD START ------------------------------------
      *     MOVE   SYS-1001-HOSPID       TO    RECE10-HOSPID.
           MOVE    SPA-HOSPNUM          TO    RECE10-HOSPNUM
      *---(01.00.02) LINE ADD END   ------------------------------------
           MOVE   LNK-PRTKANRI-SRYYM    TO    RECE10-SKYYM.
      *
           IF    WRK-PARA-HKNJANUM  NOT = ZERO
               MOVE    WRK-PARA-HKNJANUM  TO  RECE10-XXSTHKNJANUM
                                              RECE10-XXEDHKNJANUM
           ELSE
               MOVE    SPACE            TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"       TO  RECE10-XXEDHKNJANUM
           END-IF
      *
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
      *---(01.00.02) LINE ADD START ------------------------------------
      *     MOVE    WRK-PATH            TO  MCP-PATH
      *     CALL    "MCPSUB"                USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
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
      *---(01.00.02) LINE ADD START ------------------------------------
      *     MOVE    WRK-PATH            TO  MCP-PATH
      *     MOVE    "DBFETCH"           TO  MCP-FUNC
      *     CALL    "MCPSUB"                USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
           MOVE    "tbl_seikyu"            TO  MCP-TABLE
           MOVE    WRK-PATH                TO  MCP-PATHNAME
           MOVE    "DBFETCH"               TO  MCP-FUNC
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE10-REC
      *
      *        テスト患者のときは読み飛ばす
               MOVE    SPACE               TO  PTINF-REC
               INITIALIZE                      PTINF-REC
      *---(01.00.02) LINE ADD START ------------------------------------
      *         MOVE    RECE10-HOSPID       TO  PTINF-HOSPID
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
      *---(01.00.02) LINE ADD END   ------------------------------------
               MOVE    RECE10-PTID         TO  PTINF-PTID
               MOVE    PTINF-REC           TO  MCPDATA-REC
               PERFORM 800-PTINF-READ-SEC
               IF      FLG-PTINF           =   ZERO
               AND     PTINF-TSTPTNUMKBN   =   "1"
                   GO  TO  900-SEIKYU-READ-SEC
               END-IF
      *        県内データは対象外にする
               IF  RECE10-PREFKBN     NOT  =   "2"
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
      *---(01.00.02) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTINF-KEY         TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
      *---(01.00.02) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-PTINF-KEY         TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
      *                                      MCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
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
      *---(01.00.02) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTINF-KEY         TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
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
           READ    SKT4520TMP-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-SKT4520TMP
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-SKT4520TMP
           END-READ
           .
       800-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  SKT4520TMP-FILE   NEXT
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
      *---(01.00.02) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-SYSKANRI-KEY10      TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_syskanri"           TO  MCP-TABLE
           MOVE    "key10"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                    TO  MCP-FUNC
      *---(01.00.02) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-SYSKANRI-KEY10      TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
      *                                      MCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_syskanri"       TO  MCP-TABLE
               MOVE    "key10"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
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
      *---(01.00.02) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-SYSKANRI-KEY10      TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_syskanri"       TO  MCP-TABLE
           MOVE    "key10"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
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
      *---(01.00.02) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-SEIKYU-KEY          TO  MCP-PATH
      *     CALL    "MCPSUB"            USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                TO  MCP-FUNC
      *---(01.00.02) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-SEIKYU-KEY      TO  MCP-PATH
      *         CALL    "MCPSUB"            USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
               MOVE    "tbl_seikyu"         TO  MCP-TABLE
               MOVE    "key"                TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"          USING
                                            MCPAREA
                                            MCPDATA-REC
                                            SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
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
      *---(01.00.02) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-SEIKYU-KEY                TO  MCP-PATH
      *     CALL    "MCPSUB"            USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
           MOVE    "tbl_seikyu"         TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
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
      *---(01.00.02) LINE ADD START ------------------------------------
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
           MOVE    LNK-PRTKANRI-SRYYM      TO  SYS-1900-STYUKYMD(1:6)
           MOVE    "01"                    TO  SYS-1900-STYUKYMD(7:2)
           MOVE    LNK-PRTKANRI-SRYYM      TO  SYS-1900-EDYUKYMD(1:6)
           MOVE    "01"                    TO  SYS-1900-EDYUKYMD(7:2)
           MOVE    SYS-1900-REC            TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(8) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
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
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1901-STYUKYMD(7:2)
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1901-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1901-EDYUKYMD(7:2)
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
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1902-STYUKYMD(7:2)
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1902-EDYUKYMD(7:2)
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
      *---(01.00.02) LINE ADD END   ------------------------------------
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
      *---(01.00.02) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.02) LINE ADD END   ------------------------------------
           MOVE    "DBOPEN"            TO  MCP-FUNC.
      *---(01.00.02) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"            USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
      *
      *---(01.00.02) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.02) LINE ADD END   ------------------------------------
           MOVE    "DBSTART"           TO  MCP-FUNC.
      *---(01.00.02) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"            USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
      *---(01.00.02) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.02) LINE ADD END   ------------------------------------
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
      *---(01.00.02) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"            USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
      *
      *---(01.00.02) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.02) LINE ADD END   ------------------------------------
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
      *---(01.00.02) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"            USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.02) LINE ADD END   ------------------------------------
           .                             
       900-DBDISCONNECT-EXT.
           EXIT.
