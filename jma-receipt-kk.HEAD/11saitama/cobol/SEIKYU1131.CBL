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
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     SEIKYU1131.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 地方公費
      *  コンポーネント名  : 埼玉県朝霞市等４市医療費明細書
      *                      (343,342,344)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/04/12     範琳         新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev	修正者	日付		内容
      *  01.00.01    内田    07/09/23    日レセver4.0対応
      *                                  システム管理マスタ「1901」の
      *                                  医療機関情報を参照
      *                                  長期上位（974）は長期と同様の
      *                                  処理をする
      *  01.00.02    吉川    08/10/08    SPA-AREA初期化
      *  01.00.03    吉川    12/12/21    医療機関情報編集対応
      *****************************************************************
      *
       ENVIRONMENT   DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
      *
      *    明細書用ファイル
           SELECT  MF100-FILE   ASSIGN    MF100PARA
                               ORGANIZATION    IS  INDEXED
                               ACCESS  MODE    IS  DYNAMIC
                               RECORD  KEY     IS  MF100-KEY
                               FILE    STATUS  IS  STS-MF100.
      *
006900*    エラーファイル
           SELECT  RECEERR-FILE ASSIGN  RECEERR
                                FILE    STATUS  IS  STS-RECEERR.
      *
       DATA  DIVISION.
       FILE  SECTION.
      *
      *
      *    集計ファイル
       FD  MF100-FILE.
       01  MF100-REC.
           COPY  "SEI1131.INC".
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC  PIC X(200).
      *
       WORKING-STORAGE  SECTION.
      *
      *
           COPY  "CPCOMMONDAT2.INC"
                   REPLACING  //RECE01//
                   BY         //MF100//.
      *
      *    エラーファイル 名称領域
      *---(01.00.01) LINE ADD START ------------------------------------
      *     COPY  "CPCOMMONDAT2.INC"
      *             REPLACING  //RECE01PARA//
      *             BY         //RECEERR//.
      *     03      PIC X(04)   VALUE   ".dat".
           COPY    "CPERRFL.INC"    REPLACING  //ERRFLPARA//
                                    BY         //RECEERR//.
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
           COPY  "SC1131.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-MF100   PIC X(02).
           03  STS-RECE02  PIC X(02).
           03  STS-RECEERR  PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF      PIC 9(01).
           03  FLG-END        PIC 9(01).
           03  FLG-SYUNOU     PIC 9(01).
           03  FLG-HEIYOU     PIC 9(01).
           03  FLG-MF100      PIC 9(01).
           03  FLG-TYOKI      PIC 9(01).
           03  FLG-PRINT      PIC 9(01).
           03  FLG-SYSKANRI   PIC 9(01).
           03  FLG-HKNJAINF   PIC 9(01).
           03  FLG-HKNCOMBI   PIC 9(01).
           03  FLG-HANTEI     PIC 9(01).
           03  FLG-ERR        PIC 9(01).
           03  FLG-BTPARA     PIC 9(01).
           03  FLG-SYORI      PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX            PIC 9(04).
           03  IDXX           PIC 9(04).
           03  IDX1           PIC 9(04).
           03  IDX2           PIC 9(04).
           03  IDX3           PIC 9(04).
           03  IDY            PIC 9(04).
           03  IDZ            PIC 9(04).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
      *---(01.00.01) LINE ADD START ------------------------------------
      *     03  WRK-PARA-HOSPID     PIC X(24).
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *---(01.00.01) LINE ADD END   ------------------------------------
           03  WRK-PARA-PAGE       PIC 9(10).
           03  WRK-PARA-SYORIFLG   PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR           PIC  X(200).
      *
           03  WRK-Z2                PIC ZZ.
           03  WRK-Z3                PIC ZZZ.
           03  WRK-Z4                PIC ZZZZ.
           03  WRK-Z5                PIC ZZZZZ.
           03  WRK-Z6                PIC ZZZZZZ.
           03  WRK-Z7                PIC ZZZZZZ.
           03  WRK-Z8                PIC ZZZZZZZZ.
      *
           03  WRK-SRYYMWH           PIC X(16).
           03  WRK-SEIYMDWH          PIC X(22).
           03  WRK-HENYMDG           PIC X(22).
           03  WRK-HENYMDG2          PIC X(20).
           03  WRK-HENYMDG1.
               05  WRK-DAY2-EDTYMD1-G     PIC X(01).
               05  WRK-DAY2-EDTYMD1-Y     PIC Z9.
               05  WRK-DAY2-EDTYMD1-P1    PIC X(01).
               05  WRK-DAY2-EDTYMD1-M     PIC Z9.
               05  WRK-DAY2-EDTYMD1-P2    PIC X(01).
               05  WRK-DAY2-EDTYMD1-D     PIC Z9.
               05  WRK-SDD                PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-LASTYMD           PIC 9(08).
      *
           03  WRK-GPAGE               PIC 9(03).
           03  WRK-HKNJANUM            PIC X(06).
           03  WRK-KASAN-TBL.
               05  WRK-KEISAN          PIC 9(08).
               05  WRK-AMARI           PIC 9(02).
               05  WRK-KOHNUM          PIC X(03).
               05  WRK-KOHID           PIC 9(10).
               05  WRK-SEIKYUGK        PIC 9(08).
               05  WRK-SEIKYUGK-G      PIC 9(08).
      *
           03  WRK-HENSYU.
               05  WRK-JKYSNUM       PIC X(06).
               05  WRK-BYTE1         PIC 9(03).
               05  WRK-BYTE2         PIC 9(03).
               05  WRK-6FLG          PIC 9(01).
      *
           03  WRK-TAIHI-TBL.
               05  WRK-T-KOHNUM        PIC X(03).
               05  WRK-T-SEIKYUGK      PIC 9(08).
               05  WRK-T-FTNMONEY      PIC 9(08).
               05  WRK-SRYYM           PIC X(04).
           03  WRK-HKNFTN          PIC 9(05).
           03  WRK-JKYSFTN         PIC 9(04).
           03  WRK-SASHKNFTN       PIC 9(05).
      *
      *    文字変換
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
      *
      *---(01.00.01) LINE ADD START ------------------------------------
      *     03  WRK-PATH.
      *         05  FILLER              PIC S9(09)  BINARY.
      *         05  FILLER              PIC S9(09)  BINARY.
      *         05  FILLER              PIC S9(09)  BINARY.
           03  WRK-PATH              PIC X(50).
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
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
      *    小計
       01  SUM-AREA.
           03  SUM-SKENSU             PIC 9(02).
           03  SUM-SHKNFTN            PIC 9(06).
            03  SUM-SASHKNFTN          PIC 9(06).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE02  PIC 9(06).
           03  CNT-MF100   PIC 9(06).
           03  CNT-PAGE    PIC 9(03).
           03  CNT-PAGE2   PIC 9(03).
      *
      *    KEY領域
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-KOHFLG   PIC X(01).
               05  KEY-N-FTNJANUM PIC X(08).
           03  KEY-OLD.
               05  KEY-O-KOHFLG   PIC X(01).
               05  KEY-O-FTNJANUM PIC X(08).
      *
       01  TABLE-AREA.
           03  TBL-PARA-OCC    OCCURS  100.
               05  TBL-PARA-HKNJANUM    PIC X(08).
               05  TBL-PARA-PTID        PIC 9(10).
      *
       01  WRK-CONS-PARA-MAX   PIC 9(03)   VALUE   100.
      *
      *    DB参照用
      *---(01.00.01) LINE ADD START ------------------------------------
      *     COPY    "ORCA-DBPATH".
           COPY    "COMMON-SPA".
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
      *****************************************************************
      *    ファイルレイアウト　領域
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY  "CPSK1002.INC".
      *
      *---(01.00.01) LINE ADD START ------------------------------------
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    収納情報
       01  SYUNOU-REC.
           COPY  "CPSYUNOU.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC"
                   REPLACING  //KOHSKY//
                   BY         //RECE02//.
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険者情報
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    共通パラメタ
           COPY  "MCPAREA".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".      
      *
           COPY  "MCPDATA.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
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
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC  SECTION.
      *
           DISPLAY  "000-PROC-SEC       START"
           INITIALIZE  FLG-AREA
           INITIALIZE  STS-AREA
           INITIALIZE  WRK-AREA
           INITIALIZE  CNT-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  KEY-AREA
      *=== SPA-AREA初期化 by Yoshikawa(2008/10/08) start
           INITIALIZE  SPA-AREA
      *=== SPA-AREA初期化 by Yoshikawa(2008/10/08) end
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
      *---(01.00.01) LINE ADD START ------------------------------------
      *                                         WRK-PARA-HOSPID
                                               WRK-PARA-HOSPNUM
      *---(01.00.01) LINE ADD END   ------------------------------------
                                               WRK-PARA-SYORIFLG
      *---(01.00.01) LINE ADD START ------------------------------------
                                               RECEERR
      *---(01.00.01) LINE ADD END   ------------------------------------
           END-UNSTRING
      *---(01.00.01) LINE ADD START ------------------------------------
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
      *    ステップ開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *---(01.00.01) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *---(01.00.01) LINE ADD END   ------------------------------------
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "SEIKYU1131"    TO  JOB-PGID
           MOVE    
           "朝霞市等４市・医療費明細書" 
                                   TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
      *---(01.00.01) LINE ADD START ------------------------------------
                                   SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
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
           MOVE  "MF102ZZ"        TO  MF100PARA-FILE-ID
           MOVE  LNK-PRTKANRI-TERMID(1:16)  TO  MF100PARA-TERMID
      *---(01.00.01) LINE ADD START ------------------------------------
           MOVE  WRK-PARA-HOSPNUM           TO  MF100PARA-HOSPNUM
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
      *---(01.00.01) LINE ADD START ------------------------------------
      *     MOVE  "RECEERR"        TO  RECEERR-FILE-ID
      *     MOVE  LNK-PRTKANRI-TERMID(1:16)  
      *                            TO  RECEERR-TERMID
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
      *    医療機関ＩＤ編集
           INITIALIZE  SYS-1001-REC
           MOVE  "1001"        TO  SYS-1001-KANRICD
           MOVE  "*"           TO  SYS-1001-KBNCD
           MOVE  LNK-PRTKANRI-SRYYM(1:6)
                               TO  SYS-1001-STYUKYMD(1:6)
                                   SYS-1001-EDYUKYMD(1:6)
           MOVE  "01"          TO  SYS-1001-STYUKYMD(7:2)
                                   SYS-1001-EDYUKYMD(7:2)
      *---(01.00.01) LINE ADD START ------------------------------------
           MOVE  SPA-HOSPNUM   TO  SYS-1001-HOSPNUM
      *---(01.00.01) LINE ADD END   ------------------------------------
           MOVE  SYS-1001-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1001-REC
           ELSE
             DISPLAY "*** SEIKYU1131 SYS 1001 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
      *    医療機関情報−所在地、連絡先
           INITIALIZE  SYS-1002-REC
           MOVE  "1002"        TO  SYS-1002-KANRICD
           MOVE  "*"           TO  SYS-1002-KBNCD
           MOVE  LNK-PRTKANRI-SRYYM(1:6)
                               TO  SYS-1002-STYUKYMD(1:6)
                                   SYS-1002-EDYUKYMD(1:6)
           MOVE  "01"          TO  SYS-1002-STYUKYMD(7:2)
                                   SYS-1002-EDYUKYMD(7:2)
      *---(01.00.01) LINE ADD START ------------------------------------
           MOVE  SPA-HOSPNUM   TO  SYS-1002-HOSPNUM
      *---(01.00.01) LINE ADD END   ------------------------------------
           MOVE  SYS-1002-REC  TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF  FLG-SYSKANRI  =  ZERO
             MOVE  MCPDATA-REC  TO  SYS-1002-REC
           ELSE
             DISPLAY "*** SEIKYU1131 SYS 1002 ERR ***"
             MOVE  1            TO  FLG-END
           END-IF
      *
      *---(01.00.01) LINE ADD START ------------------------------------
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1002-ADRS       TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
      *    パラメタ編集処理
           PERFORM 1001-PARA-HENSYU-SEC
      *
           PERFORM 900-KOHSKY-START-SEC
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
      *      IF      WRK-PARA-SYORIFLG   =   "1" 
               INITIALIZE                      BTPARA-REC
      *---(01.00.01) LINE ADD START ------------------------------------
               MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
      *---(01.00.01) LINE ADD END   ------------------------------------
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
               MOVE    BTPARA-REC          TO  MCPDATA-REC
      *---(01.00.01) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-BTPARA-KEY5
      *                                     TO  MCP-PATH
      *         MOVE    "DBSELECT"          TO  MCP-FUNC
      *         CALL    "MCPSUB"            USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
               IF      MCP-RC              =   ZERO
      *---(01.00.01) LINE ADD START ------------------------------------
      *             MOVE    PATH-TBL-BTPARA-KEY5    TO  MCP-PATH
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
      *---(01.00.01) LINE ADD END   ------------------------------------
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
      *
      *---(01.00.01) LINE ADD START ------------------------------------
      *              MOVE    PATH-TBL-BTPARA-KEY5    TO  MCP-PATH
                   MOVE    "tbl_btpara"    TO  MCP-TABLE
                   MOVE    "key5"          TO  MCP-PATHNAME
      *---(01.00.01) LINE ADD END   ------------------------------------
                   PERFORM 900-BTPARA-READ-N-SEC
               END-PERFORM
      *
      *---(01.00.01) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-BTPARA-KEY5
      *                                     TO  MCP-PATH
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
      *---(01.00.01) LINE ADD END   ------------------------------------
               MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *---(01.00.01) LINE ADD START ------------------------------------
      *         CALL    "MCPSUB"            USING
      *                                     MCPAREA
      *                                     MCPDATA-REC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
               IF      IDXX            =   ZERO
                   MOVE    ZERO            TO  WRK-PARA-SYORIFLG
               END-IF
      *     END-IF 
           .
       1001-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC  SECTION.
      *
        DISPLAY  "200-MAIN-SEC    START"
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
      *    集計処理
      *****************************************************************
       2001-SYUKEI-SEC  SECTION.
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       4
               IF      RECE02-KOHNUM(IDZ)  =   "343" OR "342" OR "344"
                   PERFORM  20011-SYUKEI-SEC
               END-IF
           END-PERFORM
      *
           PERFORM  900-KOHSKY-READ-SEC
      *
           .
       2001-SYUKEI-EXT.
           EXIT.
      **
      *****************************************************************
      *    集計処理２
      *****************************************************************
       20011-SYUKEI-SEC  SECTION.
      *
      *    TMP-FILE キーセット
      *
           INITIALIZE       MF100-REC
      *
      *---(01.00.01) LINE ADD START ------------------------------------
      *     MOVE  RECE02-HOSPID       TO  MF100-HOSPID
           MOVE  SPA-HOSPNUM         TO  MF100-HOSPNUM
      *---(01.00.01) LINE ADD END   ------------------------------------
           MOVE  RECE02-PTNUM        TO  MF100-PTNUM
           MOVE  RECE02-RECEKA       TO  MF100-SRYKA
           MOVE  RECE02-HKNJANUM-KEY TO  MF100-HKNJANUM-KEY
           MOVE  RECE02-TEKSTYMD     TO  MF100-TEKSTYMD
           MOVE  RECE02-NYUGAIKBN    TO  MF100-NYUGAIKBN
           MOVE  RECE02-SRYYM        TO  MF100-SRYYM
      *
           MOVE  RECE02-PTID         TO  MF100-PTID
           MOVE  RECE02-NAME         TO  MF100-NAME
           MOVE  RECE02-KANANAME     TO  MF100-KANANAME
           MOVE  RECE02-BIRTHDAY     TO  MF100-BIRTHDAY
           MOVE  RECE02-SEX          TO  MF100-SEX
           MOVE  RECE02-AGE          TO  MF100-AGE
           MOVE  RECE02-HKN          TO  MF100-HKN
           MOVE  RECE02-RJNHKN       TO  MF100-RJNHKN
      *
           MOVE  RECE02-KOHINF      (IDZ)  TO  MF100-KOHINF
           MOVE  RECE02-KOHJKYSNUM  (IDZ)  TO  MF100-JKYSNUM
           MOVE  RECE02-KOHFTNJANUM (IDZ)  TO  MF100-FTNJANUM
           EVALUATE  MF100-KOHNUM
           WHEN  "343"
               MOVE  1                     TO  MF100-KOHFLG
           WHEN  "344"
               MOVE  2                     TO  MF100-KOHFLG
           WHEN  "342"
               MOVE  3                     TO  MF100-KOHFLG
           END-EVALUATE
           COMPUTE IDY     =         IDZ   +   1
           MOVE  RECE02-JNISSU      (IDY)  TO  MF100-JNISSU
           MOVE  RECE02-TOTALTEN    (IDY)  TO  MF100-TOTALTEN
           MOVE  RECE02-FTNMONEY    (IDY)  TO  MF100-FTNMONEY
      *    
      *    患者データ区分設定
           MOVE      1                TO    MF100-GOKEIKBN
      *
      *    収納情報より助成の判断、保険診療一部負担金を集計
           INITIALIZE                       WRK-KASAN-TBL
           MOVE  ZERO                 TO    FLG-TYOKI
                                            FLG-ERR
                                            FLG-HANTEI
           MOVE    MF100-KOHNUM       TO    WRK-KOHNUM
           MOVE    MF100-KOHID        TO    WRK-KOHID
      *
      *  ３４４(本人)の時は助成判定をする為に FLG-ERR を 1 にする
           IF    WRK-KOHNUM        =  "344"
           AND   RECE02-SHOKUJIFTNKBN  =  SPACE
                 MOVE  1                    TO  FLG-ERR
                                                FLG-HANTEI
           ELSE
      *  ３４３・３４２・３４４(低所得)で、患者が支払をしている場合は
      *  助成対象外にする
             IF  RECE02-FTNMONEY(IDY)    >    ZERO
               MOVE  1                      TO  FLG-ERR
             END-IF
           END-IF
      *
           IF      RECE02-NYUGAIKBN    =   "1"
      *---(01.00.01) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-SYUNOU-KEY33   TO  WRK-PATH
               MOVE    "key33"                 TO  WRK-PATH
      *---(01.00.01) LINE ADD END   ------------------------------------
           ELSE
      *---(01.00.01) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-SYUNOU-KEY25   TO  WRK-PATH
               MOVE    "key25"                 TO  WRK-PATH
      *---(01.00.01) LINE ADD END   ------------------------------------
           END-IF
      *
           PERFORM  920-SYUNOU-SET-SEC
      *
      *    保険限度額
           IF  RECE02-FTNMONEY(1)  >  ZERO
           AND WRK-SEIKYUGK-G      >  RECE02-FTNMONEY(1) 
               MOVE    RECE02-FTNMONEY(1)  TO  WRK-SEIKYUGK-G
           END-IF
      *
      *    長期の患者に限度額１万円を設定
           IF    FLG-TYOKI          =  1  AND
                 WRK-SEIKYUGK-G    >=  10000
               MOVE    10000          TO    WRK-SEIKYUGK-G
           END-IF
      *---(01.00.01) LINE ADD START ------------------------------------
      *    長期上位
           IF    FLG-TYOKI          =  2  AND
                 WRK-SEIKYUGK-G    >=  20000
               MOVE    20000          TO    WRK-SEIKYUGK-G
           END-IF
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
      *    一部負担金等の額、他法点数設定
           MOVE     WRK-SEIKYUGK-G    TO    MF100-SEIKYUGK
      *
           MOVE     MF100-KOHNUM      TO    WRK-T-KOHNUM
           MOVE     MF100-SEIKYUGK    TO    WRK-HKNFTN
           MOVE     MF100-FTNMONEY    TO    WRK-T-FTNMONEY
           COMPUTE  WRK-T-SEIKYUGK  =  WRK-HKNFTN
                                    -  WRK-T-FTNMONEY
      *
      *    保険者番号チェック処理
           PERFORM 200111-HKNJANUM-CHK-SEC
      *
           IF  FLG-ERR   =    ZERO
      *
             WRITE    MF100-REC
             ADD      1                 TO    CNT-MF100
      *
      *    合計ファイル編集
             PERFORM  20011-GOKEI-HEN-SEC
           ELSE
             DISPLAY "助成対象外患者番号="  RECE02-PTNUM
             DISPLAY "患者負担額 ="  RECE02-FTNMONEY(IDY)
           END-IF
      *
      *
           .
       20011-SYUKEI-EXT.
           EXIT.
      **
      *****************************************************************
      *    合計編集処理
      *****************************************************************
       20011-GOKEI-HEN-SEC                SECTION.
      *
           INITIALIZE      MF100-REC
      *
      *    KEY設定
           MOVE   ZERO                         TO  MF100-GOKEIKBN
      *---(01.00.01) LINE ADD START ------------------------------------
      *     MOVE   RECE02-HOSPID                TO  MF100-HOSPID
           MOVE   SPA-HOSPNUM                  TO  MF100-HOSPNUM
      *---(01.00.01) LINE ADD END   ------------------------------------
           MOVE   RECE02-KOHFTNJANUM    (IDZ)  TO  MF100-FTNJANUM
           EVALUATE  WRK-T-KOHNUM
           WHEN  "343"
               MOVE  1                         TO  MF100-KOHFLG
           WHEN  "344"
               MOVE  2                         TO  MF100-KOHFLG
           WHEN  "342"
               MOVE  3                         TO  MF100-KOHFLG
           END-EVALUATE
      *    一時ファイル読み込み
           PERFORM  800-SKTTMP-READ-SEC
      *
           ADD     1                  TO    MF100-GKENSU
           ADD     WRK-T-SEIKYUGK     TO    MF100-GSEIKYUGK
           ADD     WRK-T-FTNMONEY     TO    MF100-GFTNMONEY
      *
           IF      FLG-MF100  =  1
                   WRITE    MF100-REC
           ELSE
                   REWRITE  MF100-REC
           END-IF
      *
           .
       20011-GOKEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別発行時の保険者番号(市町村番号)チェック処理
      *****************************************************************
       200111-HKNJANUM-CHK-SEC                SECTION.
      *
           INITIALIZE      WRK-HKNJANUM
           MOVE    "11"                     TO  WRK-HKNJANUM(1:2)
           MOVE    MF100-KOHFTNJANUM        TO  WRK-HKNJANUM(3:3)
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
      *
                   DISPLAY  "TBL-PARA-HKNJANUM = " 
                                               TBL-PARA-HKNJANUM(IDXX)
      *
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
       2002-PRINT-SEC                SECTION.
      *
           OPEN     INPUT     MF100-FILE
           MOVE     ZERO      TO  FLG-PRINT
      *
      *    一時ファイル読込
           PERFORM  900-TMP-READ-SEC
      *
           PERFORM  UNTIL     FLG-PRINT   =  1
      *
               MOVE  KEY-NEW    TO  KEY-OLD
      *    帳票編集<見出し>処理
               PERFORM  310-HEAD-HEN-SEC
      *    帳票編集<明細>処理
               PERFORM  320-BODY-HEN-SEC
      *    帳票編集<合計>処理
               PERFORM  3201-GOKEI-HEN-SEC
      *    帳票印刷処理
               PERFORM  330-PRINT-OUT-SEC
           END-PERFORM
      *
           CLOSE    MF100-FILE
      *
           .
       2002-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    総枚数集計処理
      *****************************************************************
       20011-MAISU-SYUKEI-SEC                SECTION.
      *
           MOVE  ZERO                  TO  WRK-KEISAN
                                           WRK-AMARI
                                           WRK-GPAGE
           DIVIDE   MF100-GKENSU       BY  8
                    GIVING             WRK-KEISAN
                    REMAINDER          WRK-AMARI
           MOVE  WRK-KEISAN             TO  WRK-GPAGE
           IF  WRK-AMARI  >  0
             ADD  1                     TO  WRK-GPAGE
           END-IF
      *
           .
       20011-MAISU-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HEAD-HEN-SEC          SECTION.
      *
      *
           ADD     1    TO    CNT-PAGE
                              CNT-PAGE2
      *
           INITIALIZE                             SC31
      *
      *    保険種類
           EVALUATE  MF100-KOHFLG
             WHEN  "1"
               MOVE  "レ"                    TO  SC31-HKNMEI(1)
             WHEN  "2"
               MOVE  "レ"                    TO  SC31-HKNMEI(2)
             WHEN  "3"
               MOVE  "レ"                    TO  SC31-HKNMEI(3)
           END-EVALUATE
      *
      *    請求先市長名
           MOVE    "11"                    TO  WRK-HKNJANUM(1:2)
           MOVE     MF100-FTNJANUM         TO  WRK-HKNJANUM(3:3)
           PERFORM  920-HKNJAINF-SET-SEC
           IF       HKNJA-HKNJANAME        =   SPACE
             MOVE   "市長"                 TO  SC31-SITYOMEI
      *
             MOVE   SPACE                  TO  SC31-SITYOMEI1
      *
           ELSE
             STRING HKNJA-HKNJANAME        DELIMITED  BY  SPACE
                    "長"                   DELIMITED  BY  SPACE
                    INTO                   SC31-SITYOMEI
             END-STRING
      *
             MOVE    SC31-SITYOMEI(1:6)    TO SC31-SITYOMEI1
      *
           END-IF
      *
      *    印刷年月日
           MOVE    WRK-SEIYMDWH            TO  SC31-SEIYMD
      *---(01.00.01) LINE ADD START ------------------------------------
      *     MOVE    SYS-1001-HOSPNAME       TO  SC31-HOSPNAME
      *    医療機関名
           MOVE    WRK-HOSPNAME            TO  SC31-HOSPNAME
      *=== 医療機関情報編集修正 by Yoshikawa(2012/12/21) start
           INSPECT SC31-HOSPNAME           REPLACING ALL "  " BY "　"
      *=== 医療機関情報編集修正 by Yoshikawa(2012/12/21) end
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
      *    開設者名
           MOVE    SYS-1001-KAISETUNAME    TO  SC31-DAIHYOUNAME
      *
      *    合計編集
           IF    MF100-GOKEIKBN    =    ZERO
      *  合計件数
             MOVE  MF100-GKENSU            TO  WRK-Z4
             MOVE  WRK-Z4                  TO  SC31-GOKEI
      *
      *  合計 医療機関ーー市請求額
             MOVE  MF100-GSEIKYUGK         TO  WRK-Z8
             MOVE  WRK-Z8                  TO  SC31-SAGHKNFTN
      *
             MOVE  1                       TO  CNT-PAGE2
             PERFORM  20011-MAISU-SYUKEI-SEC
      *
             PERFORM  900-TMP-READ-SEC
           END-IF
      *
      *    ページ数
           MOVE    CNT-PAGE2               TO  WRK-Z3
           MOVE    WRK-Z3                  TO  SC31-PAGE
      *
      *    ページ総数
           MOVE   WRK-GPAGE                TO  WRK-Z3
           MOVE   WRK-Z3                   TO  SC31-ZPAGE
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
           INITIALIZE         SUM-AREA
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >      8
      *
                   OR      FLG-PRINT       =   1
                   OR      KEY-NEW     NOT =   KEY-OLD
      *
      *    受給者番号(右詰めにして、スペースは０で埋める)
             INITIALIZE  WRK-HENSYU WRK-SASHKNFTN
             PERFORM    VARYING   IDX3       FROM     1  BY  1
                        UNTIL     IDX3          >    6
               IF  MF100-JKYSNUM(IDX3:1)  =  SPACE
                   COMPUTE  WRK-BYTE1  =  IDX3 - 1
                   COMPUTE  WRK-BYTE2  =  (6 - WRK-BYTE1) + 1
                   MOVE     6            TO  IDX3
                   MOVE     1            TO  WRK-6FLG
               END-IF
             END-PERFORM
             IF  WRK-6FLG           =     ZERO
               MOVE    MF100-JKYSNUM     TO  WRK-JKYSNUM
             ELSE
               IF  MF100-JKYSNUM  NOT =  SPACE
                 MOVE    MF100-JKYSNUM(1:WRK-BYTE1)
                                   TO  WRK-JKYSNUM(WRK-BYTE2:WRK-BYTE1)
               END-IF
             END-IF
             INSPECT  WRK-JKYSNUM  REPLACING  ALL " "  BY  "0"
             MOVE     WRK-JKYSNUM           TO  WRK-MAE-INPUT
             PERFORM  2009-HENKAN-SEC
             MOVE     WRK-OUT-INPUT         TO  SC31-JKYSNUM(IDX1)
      *
      *    受給者氏名
               MOVE    MF100-KANANAME       TO  SC31-KATAKANA(IDX1)
      *
      *    生年月日
               MOVE    MF100-BIRTHDAY      TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG1        TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               INSPECT WRK-OUT-INPUT    REPLACING  ALL "　"  BY  "０"
               MOVE    WRK-OUT-INPUT(1:2)   TO
                                            SC31-BRITHDAYH(IDX1)
               MOVE    WRK-OUT-INPUT(3:2)   TO
                                            SC31-BRITHDAYY1(IDX1)
               MOVE    WRK-OUT-INPUT(5:2)   TO
                                            SC31-BRITHDAYY2(IDX1)
               MOVE    WRK-OUT-INPUT(9:2)   TO
                                            SC31-BRITHDAYM1(IDX1)
               MOVE    WRK-OUT-INPUT(11:2)  TO
                                            SC31-BRITHDAYM2(IDX1)
               MOVE    WRK-OUT-INPUT(15:2)  TO
                                            SC31-BRITHDAYD1(IDX1)
               MOVE    WRK-OUT-INPUT(17:2)  TO
                                            SC31-BRITHDAYD2(IDX1)
      *
      *         MOVE    WRK-OUT-INPUT(9:4)  TO  SC31-BRITHDAY(IDX1)(7:4)
      *         MOVE    WRK-OUT-INPUT(15:4) TO  SC31-BRITHDAY(IDX1)(11:4)
      *
      *    診療年月
               MOVE    MF100-SRYYM         TO  WRK-SYMD(1:6)
               MOVE    "01"                TO  WRK-SYMD(7:2)
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG1        TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               INSPECT WRK-OUT-INPUT    REPLACING  ALL  "　"  BY  "０"
               MOVE    WRK-OUT-INPUT(3:2)   TO  SC31-SRYYMY1(IDX1)
               MOVE    WRK-OUT-INPUT(5:2)   TO  SC31-SRYYMY2(IDX1)
               MOVE    WRK-OUT-INPUT(9:2)   TO  SC31-SRYYMM1(IDX1)
               MOVE    WRK-OUT-INPUT(11:2)  TO  SC31-SRYYMM2(IDX1)
      * 年月の記入
               MOVE    WRK-OUT-INPUT(1:2)  TO WRK-SRYYM
               EVALUATE  WRK-SRYYM
                 WHEN "Ｈ"
                     MOVE "平成"           TO SC31-SRYYMH(IDX1)
                 WHEN "ｓ"
                     MOVE "昭和"           TO SC31-SRYYMH(IDX1)
               END-EVALUATE
      *         MOVE    WRK-OUT-INPUT(9:4)  TO  SC31-SRYYM(IDX1)(7:4)
      *
      *    診療日数
               MOVE    MF100-JNISSU        TO  WRK-Z2
               MOVE    WRK-Z2              TO  SC31-NITISU-TBL(IDX1)
      *
      *    点数
               MOVE    MF100-TOTALTEN      TO  WRK-Z6
               MOVE    WRK-Z6              TO  SC31-HKSRYTEN(IDX1)
      *
      *    他法点数
               MOVE    MF100-TAHOU         TO  WRK-Z4
               MOVE    WRK-Z4              TO  SC31-TAHOTENSU(IDX1)
      *
      *    一部負担金
               MOVE    MF100-SEIKYUGK      TO  WRK-Z5
               MOVE    WRK-Z5              TO  SC31-HKNFTN(IDX1)
               MOVE    MF100-SEIKYUGK      TO  WRK-HKNFTN
      *
      *    本人負担額
               MOVE    MF100-FTNMONEY      TO  WRK-Z4
               MOVE    WRK-Z4              TO  SC31-JKYSFTN(IDX1)
               MOVE    MF100-FTNMONEY      TO  WRK-JKYSFTN
      *    医療機関--市 請求額
              COMPUTE WRK-SASHKNFTN = WRK-HKNFTN
                                    - WRK-JKYSFTN
               MOVE    WRK-SASHKNFTN     TO  WRK-Z5
               MOVE    WRK-Z5              TO  SC31-SASHKNFTN(IDX1)
      *    小計
      *    件数
               ADD     1                   TO  SUM-SKENSU
      *    医療機関--市 請求額
                ADD     WRK-SASHKNFTN         TO  SUM-SASHKNFTN 
      *     レセプト明細読込
                 PERFORM 900-TMP-READ-SEC
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *              帳票編集<合計>処理
      *****************************************************************
       3201-GOKEI-HEN-SEC  SECTION.
      *
      *     小計
      *     件数
            MOVE    SUM-SKENSU             TO  WRK-Z2
            MOVE    WRK-Z2                 TO  SC31-SYKEI
      *     医療機関ーー市請求額   
            MOVE    SUM-SASHKNFTN          TO  WRK-Z7
            MOVE    WRK-Z7                 TO  SC31-SASJKYSFTN
      *
           .
       3201-GOKEI-HEN-EXT.
           EXIT.
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
           MOVE   "SC1131.red"         TO  SPRT-PRTID
           MOVE    "朝霞市等４市・医療費明細書"
                                    TO  SPRT-TITLE
           MOVE    SC31             TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
      *---(01.00.01) LINE ADD START ------------------------------------
                                       SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
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
           END-IF
      *
           CLOSE  RECEERR-FILE
      *
           .
       500-ERR-HENSYU-EXT.
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
           MOVE    LNK-DAY2-EDTYMD2    TO  WRK-HENYMDG2
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG1
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           INITIALIZE                      HKNCOMBI-REC
      *---(01.00.01) LINE ADD START ------------------------------------
      *     MOVE    SYU-HOSPID              TO  COMB-HOSPID
           MOVE    SPA-HOSPNUM             TO  COMB-HOSPNUM
      *---(01.00.01) LINE ADD END   ------------------------------------
           MOVE    SYU-PTID                TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM         TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
      *---(01.00.01) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-HKNCOMBI-KEY   TO  MCP-PATH
      *     CALL    "MCPSUB"                    USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"               TO  MCP-FUNC
      *---(01.00.01) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-HKNCOMBI-KEY   TO  MCP-PATH
      *         CALL    "MCPSUB"                    USING
      *                                             MCPAREA
      *                                             MCPDATA-REC
               MOVE    "tbl_hkncombi"        TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"           USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
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
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
      *---(01.00.01) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-HKNCOMBI-KEY   TO  MCP-PATH
      *     CALL    "MCPSUB"                    USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
           MOVE    "tbl_hkncombi"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    請求点数集計処理
      *****************************************************************
       800-SEIKYU-SEC  SECTION.
      *
           MOVE    ZERO     TO     FLG-HEIYOU
                                   WRK-SEIKYUGK
                                   WRK-KEISAN
                                   WRK-AMARI
      *
           PERFORM  VARYING  IDX  FROM  1  BY  1
                    UNTIL    IDX  >    4 OR
                    SYU-KOH-HKNNUM(IDX)  =  SPACE
             IF    SYU-KOH-HKNNUM(IDX)(1:1) =  "0"  AND
                   SYU-KOH-HKNNUM(IDX) NOT  =  "027"
      *
               MOVE   1                TO    FLG-HEIYOU
               MOVE   SYU-KOH-COMPFTN(IDX)
                                       TO    WRK-SEIKYUGK
               ADD    SYU-TOTAL-HKNTEN
                                       TO    MF100-TAHOU
      *
               DIVIDE  WRK-SEIKYUGK    BY  8
                       GIVING              WRK-KEISAN
                       REMAINDER           WRK-AMARI
               COMPUTE WRK-SEIKYUGK    =   WRK-SEIKYUGK
                                       -   WRK-AMARI
               IF  WRK-AMARI       >=    5
                 ADD    10               TO  WRK-SEIKYUGK
               END-IF
             END-IF
      *
      *計算負担金に差がある場合、地方公費の助成があったと判断する。
             IF  FLG-HANTEI             =  1    AND
                 SYU-KOH-HKNNUM(IDX)    =  WRK-KOHNUM
               IF  IDX                  =1
                 IF  SYU-SYUCOMPFTN-ENTANI
                                        >  SYU-KOH-COMPFTN-ENTANI(IDX)
                     MOVE    ZERO       TO  FLG-ERR
                 END-IF
               ELSE
                 IF  SYU-KOH-COMPFTN-ENTANI(IDX - 1)
                                         >  SYU-KOH-COMPFTN-ENTANI(IDX)
                     MOVE    ZERO        TO  FLG-ERR
                 END-IF
               END-IF
             END-IF
      *
             IF    SYU-KOH-HKNNUM(IDX)   =   "972"
               MOVE    1     TO    FLG-TYOKI
             END-IF
           END-PERFORM
      *
           IF    FLG-HEIYOU    =    ZERO
             IF    RECE02-RJNHKNNUM   =  SPACE
                 MOVE  SYU-SYUCOMPFTN    TO  WRK-SEIKYUGK
             ELSE
                 MOVE  SYU-KOH-COMPFTN(1)
                                         TO  WRK-SEIKYUGK
             END-IF
           END-IF
      *
           ADD            WRK-SEIKYUGK        TO    WRK-SEIKYUGK-G
      *
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
      *---(01.00.01) LINE ADD START ------------------------------------
      *       MOVE  RECE02-HOSPID      TO  SYU-HOSPID
             MOVE  SPA-HOSPNUM        TO  SYU-HOSPNUM
      *---(01.00.01) LINE ADD END   ------------------------------------
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
      *---(01.00.01) LINE ADD START ------------------------------------
      *       MOVE  WRK-PATH           TO  MCP-PATH
      *       CALL  "MCPSUB"           USING
      *                                MCPAREA
      *                                MCPDATA-REC
             MOVE  "tbl_syunou"       TO  MCP-TABLE
             MOVE  WRK-PATH           TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"        USING
                                      MCPAREA
                                      MCPDATA-REC
                                      SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
             IF  MCP-RC  =  ZERO
      *---(01.00.01) LINE ADD START ------------------------------------
      *         MOVE  WRK-PATH         TO  MCP-PATH
               MOVE  "tbl_syunou"       TO  MCP-TABLE
               MOVE  WRK-PATH           TO  MCP-PATHNAME
      *---(01.00.01) LINE ADD END   ------------------------------------
               PERFORM  925-SYUNOU-READ-SEC
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
      *---(01.00.01) LINE ADD START ------------------------------------
      *           MOVE  WRK-PATH            TO  MCP-PATH
                 MOVE  "tbl_syunou"     TO  MCP-TABLE
                 MOVE  WRK-PATH         TO  MCP-PATHNAME
      *---(01.00.01) LINE ADD END   ------------------------------------
                 PERFORM   925-SYUNOU-READ-SEC
               END-PERFORM
             ELSE
               DISPLAY  "SYUNOU SELECT ERR"
             END-IF
      *
             INITIALIZE  MCPAREA
             MOVE  "DBCLOSECURSOR"      TO  MCP-FUNC
      *---(01.00.01) LINE ADD START ------------------------------------
      *       MOVE  WRK-PATH             TO  MCP-PATH
      *       CALL  "MCPSUB"     USING
      *                          MCPAREA
      *                          MCPDATA-REC
             MOVE  "tbl_syunou"         TO  MCP-TABLE
             MOVE  WRK-PATH             TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"          USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
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
           MOVE  "DBFETCH"  TO  MCP-FUNC
      *---(01.00.01) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC
           CALL  "ORCDBMAIN"    USING
                                MCPAREA
                                MCPDATA-REC
                                SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC   SECTION.
      *
      *
           CLOSE  MF100-FILE
           MOVE  CNT-PAGE  TO  WRK-PARA-PAGE
      *
           DISPLAY "*** SEIKYU1131 IN  "  CNT-RECE02
           DISPLAY "*** SEIKYU1131 OUT "  CNT-MF100
           DISPLAY "*** SEIKYU1131 END ***"
      *
           IF  CNT-MF100  =  0
             DISPLAY "DATA NOT FOUND. PRINT JOB CANCEL!!"
           END-IF
      *
      *    ステップ終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *---(01.00.01) LINE ADD START ------------------------------------
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
      *---(01.00.01) LINE ADD END   ------------------------------------
           MOVE    WRK-PARA-PAGE   TO  JOB-UPDCNT
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
      *---(01.00.01) LINE ADD START ------------------------------------
                                   SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル読込
      *****************************************************************
       900-TMP-READ-SEC                SECTION.
      *
           READ  MF100-FILE   NEXT
               AT  END
                   MOVE  1  TO  FLG-PRINT
               NOT AT END
                   MOVE  MF100-KOHFLG     TO     KEY-N-KOHFLG
                   MOVE  MF100-FTNJANUM   TO     KEY-N-FTNJANUM
           END-READ
           .
       900-TMP-READ-EXT.
           EXIT.
      *      
      *****************************************************************
      *    集計ファイル読込
      *****************************************************************
       800-SKTTMP-READ-SEC             SECTION.
      *
           READ    MF100-FILE
               INVALID    KEY
                   MOVE    1           TO  FLG-MF100
               NOT INVALID  KEY
                   MOVE    ZERO        TO  FLG-MF100
           END-READ
           .
       800-SKTTMP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタ読込
      *****************************************************************
       900-KOHSKY-START-SEC            SECTION.
      *
           INITIALIZE                  RECE02-REC
      *---(01.00.01) LINE ADD START ------------------------------------
      *     MOVE    WRK-PARA-HOSPID     TO  RECE02-HOSPID
           MOVE    SPA-HOSPNUM         TO  RECE02-HOSPNUM
      *---(01.00.01) LINE ADD END   ------------------------------------
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE02-SKYYM
           MOVE    RECE02-REC          TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"                 TO  MCP-FUNC
      *---(01.00.01) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-KOHSKY-KEY2       TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               PERFORM 900-KOHSKY-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-END
               PERFORM  900-KOHSKY-CLOSE-SEC
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
           MOVE    "DBFETCH"                  TO  MCP-FUNC
      *---(01.00.01) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-KOHSKY-KEY2       TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-END
               MOVE    MCPDATA-REC     TO  RECE02-REC
               IF  RECE02-KOHNUM (1)  =  "343"  OR "342" OR "344"
               OR  RECE02-KOHNUM (2)  =  "343"  OR "342" OR "344"
               OR  RECE02-KOHNUM (3)  =  "343"  OR "342" OR "344"
      *        テスト患者のときは読み飛ばす
                   MOVE    SPACE               TO  PTINF-REC
                   INITIALIZE                      PTINF-REC
      *---(01.00.01) LINE ADD START ------------------------------------
      *             MOVE    RECE02-HOSPID       TO  PTINF-HOSPID
                   MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
      *---(01.00.01) LINE ADD END   ------------------------------------
                   MOVE    RECE02-PTID         TO  PTINF-PTID
                   MOVE    PTINF-REC           TO  MCPDATA-REC
                   PERFORM 800-PTINF-READ-SEC
                   IF      FLG-PTINF           =   ZERO
                   AND     PTINF-TSTPTNUMKBN   =   "1"
                       GO  TO  900-KOHSKY-READ-SEC
                   END-IF   
      *
      *         入院データは対象外
                   IF  RECE02-NYUGAIKBN  =  "1"
                       DISPLAY "入院患者番号="  RECE02-PTNUM
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
                   ADD  1  TO  CNT-RECE02
               ELSE
                 GO  TO  900-KOHSKY-READ-SEC
               END-IF
           ELSE
               MOVE    1               TO  FLG-END
               PERFORM  900-KOHSKY-CLOSE-SEC
           END-IF
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書マスタCLOSE
      *****************************************************************
       900-KOHSKY-CLOSE-SEC  SECTION.
      *
           MOVE    "DBCLOSECURSOR"         TO  MCP-FUNC
      *---(01.00.01) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-KOHSKY-KEY2    TO  MCP-PATH
      *     CALL    "MCPSUB"                    USING
      *                                         MCPAREA
      *                                         MCPDATA-REC
           MOVE    "tbl_kohsky"          TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
           .
       900-KOHSKY-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ読込
      *****************************************************************
       800-PTINF-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"                 TO  MCP-FUNC
      *---(01.00.01) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTINF-KEY         TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptinf"           TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"                  TO  MCP-FUNC
      *---(01.00.01) LINE ADD START ------------------------------------
      *         MOVE    PATH-TBL-PTINF-KEY         TO  MCP-PATH
      *         CALL    "MCPSUB"             USING
      *                                      MCPAREA
      *                                      MCPDATA-REC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
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
      *---(01.00.01) LINE ADD START ------------------------------------
      *     MOVE    PATH-TBL-PTINF-KEY         TO  MCP-PATH
      *     CALL    "MCPSUB"             USING
      *                                  MCPAREA
      *                                  MCPDATA-REC
           MOVE    "tbl_ptinf"            TO  MCP-TABLE
           MOVE    "key"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"            USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
           .
       800-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *   保険者情報セット
      *****************************************************************
       920-HKNJAINF-SET-SEC  SECTION.
      *
           INITIALIZE  HKNJAINF-REC
      *---(01.00.01) LINE ADD START ------------------------------------
      *     MOVE  SYS-1001-HOSPID           TO  HKNJA-HOSPID
           MOVE  SPA-HOSPNUM               TO  HKNJA-HOSPNUM
      *---(01.00.01) LINE ADD END   ------------------------------------
           MOVE  WRK-HKNJANUM              TO  HKNJA-HKNJANUM
           MOVE  HKNJAINF-REC              TO  MCPDATA-REC
           MOVE  "DBSELECT"                TO  MCP-FUNC
      *---(01.00.01) LINE ADD START ------------------------------------
      *     MOVE  PATH-TBL-HKNJAINF-KEY2    TO  MCP-PATH
      *     CALL  "MCPSUB"             USING
      *                                MCPAREA
      *                                MCPDATA-REC
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
           IF  MCP-RC  =  ZERO
      *---(01.00.01) LINE ADD START ------------------------------------
      *       MOVE  PATH-TBL-HKNJAINF-KEY2   TO  MCP-PATH
             MOVE    "tbl_hknjainf"        TO  MCP-TABLE
             MOVE    "key2"                TO  MCP-PATHNAME
      *---(01.00.01) LINE ADD END   ------------------------------------
             PERFORM  925-HKNJAINF-READ-SEC
           ELSE
             DISPLAY  "HKNJAINF SELECT ERR"
           END-IF
      *
           INITIALIZE  MCPAREA
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
      *---(01.00.01) LINE ADD START ------------------------------------
      *     MOVE  PATH-TBL-HKNJAINF-KEY2   TO  MCP-PATH
      *     CALL  "MCPSUB"         USING
      *                            MCPAREA
      *                            MCPDATA-REC
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key2"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
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
           MOVE  "DBFETCH"  TO  MCP-FUNC
      *---(01.00.01) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC
           CALL    "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
           IF  MCP-RC  =  ZERO
             MOVE  MCPDATA-REC  TO  HKNJAINF-REC
             MOVE  ZERO         TO  FLG-HKNJAINF
      *
      *    国保でない時は、読み直す
             IF    HKNJA-HKNNUM  NOT  =  "060" AND "067"
      *---(01.00.01) LINE ADD START ------------------------------------
      *         MOVE  PATH-TBL-HKNJAINF-KEY2   TO  MCP-PATH
      *---(01.00.01) LINE ADD END   ------------------------------------
               GO               TO    925-HKNJAINF-READ-SEC
             END-IF
      *
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
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC  SECTION.
      *
           MOVE  "DBSELECT"                TO  MCP-FUNC
      *---(01.00.01) LINE ADD START ------------------------------------
      *     MOVE  PATH-TBL-SYSKANRI-KEY10   TO  MCP-PATH
      *     CALL  "MCPSUB"         USING
      *                            MCPAREA
      *                            MCPDATA-REC
           MOVE  "tbl_syskanri"        TO  MCP-TABLE
           MOVE  "key10"               TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
           IF  MCP-RC  =  ZERO
             MOVE  "DBFETCH"                TO  MCP-FUNC
      *---(01.00.01) LINE ADD START ------------------------------------
      *       MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      *       CALL  "MCPSUB"        USING
      *                             MCPAREA
      *                             MCPDATA-REC
             MOVE  "tbl_syskanri"        TO  MCP-TABLE
             MOVE  "key10"               TO  MCP-PATHNAME
             CALL  "ORCDBMAIN"           USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
             IF  MCP-RC  =  ZERO
               MOVE  ZERO  TO  FLG-SYSKANRI
             ELSE
               MOVE  1  TO  FLG-SYSKANRI
             END-IF
           ELSE
             MOVE  1  TO  FLG-SYSKANRI
           END-IF
      *
           MOVE  "DBCLOSECURSOR"          TO  MCP-FUNC
      *---(01.00.01) LINE ADD START ------------------------------------
      *     MOVE  PATH-TBL-SYSKANRI-KEY10  TO  MCP-PATH
      *     CALL  "MCPSUB"        USING
      *                           MCPAREA
      *                           MCPDATA-REC
           MOVE  "tbl_syskanri"           TO  MCP-TABLE
           MOVE  "key10"                  TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"              USING
                                          MCPAREA
                                          MCPDATA-REC
                                          SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
           .
       800-SYSKANRI-READ-EXT.
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
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
      *---(01.00.01) LINE ADD START ------------------------------------
      *     CALL    "MCPSUB"            USING
      *                                 MCPAREA
      *                                 MCPDATA-REC
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
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
      *---(01.00.01) LINE ADD START ------------------------------------
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
           PERFORM 800-SYSKANRI-READ-SEC
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
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1902-STYUKYMD(7:2)
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1902-EDYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1902-EDYUKYMD(7:2)
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
      *---(01.00.01) LINE ADD END   ------------------------------------
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC  SECTION.
      *
      *---(01.00.01) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.01) LINE ADD END   ------------------------------------
           MOVE  "DBOPEN"   TO  MCP-FUNC.
      *---(01.00.01) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
      *---(01.00.01) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.01) LINE ADD END   ------------------------------------
           MOVE  "DBSTART"  TO  MCP-FUNC.
      *---(01.00.01) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC  SECTION.
      *
      *---(01.00.01) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.01) LINE ADD END   ------------------------------------
           MOVE  "DBCOMMIT"      TO  MCP-FUNC.
      *---(01.00.01) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
      *
      *---(01.00.01) LINE ADD START ------------------------------------
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *---(01.00.01) LINE ADD END   ------------------------------------
           MOVE  "DBDISCONNECT"  TO  MCP-FUNC.
      *---(01.00.01) LINE ADD START ------------------------------------
      *     CALL  "MCPSUB"     USING
      *                        MCPAREA
      *                        MCPDATA-REC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *---(01.00.01) LINE ADD END   ------------------------------------
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      
