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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBM400.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 自賠責ＰＤＦファイル作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  16/10/07    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.      
       FILE-CONTROL.
      *
      *    印刷用データ
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-PRT.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC.
           03  PRT-HEAD                PIC X(100).
           03  PRT-HEAD-R              REDEFINES   PRT-HEAD.
               05  PRT-HOSPCD          PIC X(07).
               05  PRT-NYUGAIKBN       PIC X(01).
               05  PRT-SRYYM           PIC X(06).
               05  PRT-PRTKBN          PIC X(03).
               05  PRT-MAKERCD         PIC X(04).
               05  PRT-RECENUM         PIC 9(02).
               05  PRT-PTNUM-KBN       PIC X(01).
               05  PRT-PTNUM           PIC X(20).
               05  PRT-RENNUM          PIC 9(02).
               05  FILLER              PIC X(54).
           03  PRT-PRTID               PIC X(30).
           03  PRT-PRTDATA             PIC X(20000).
      *     
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPASGNPARA.INC".
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *
       01  JIBAIPARA.
           03  JIBAIPARA-HOSPNUM       PIC X(02).
           03  JIBAIPARA-FORM-ID       PIC X(05)   VALUE   "JIBAI".
           03  JIBAIPARA-TERMID        PIC X(64).
           03  JIBAIPARA-YMD           PIC X(08).
           03  JIBAIPARA-TIME          PIC X(08).
           03  FILLER                  PIC X(04)   VALUE   ".dat".
      *
           COPY    "COMMON-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PRT                 PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-RECE10              PIC 9(01).
           03  FLG-JIBAI-PRT           PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
           03  FLG-SET                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
           03  CNT-RECE10              PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDW                     PIC 9(05).
           03  IDX                     PIC 9(05). 
           03  IDY                     PIC 9(05). 
           03  IDZ                     PIC 9(05).
           03  IDY1                    PIC 9(05).
           03  IDY2                    PIC 9(05).
           03  IDY3                    PIC 9(05).
           03  IDWW                    PIC 9(05).
           03  IDXX                    PIC 9(05).
           03  IDYY                    PIC 9(05).
           03  IDZZ                    PIC 9(05).
      *
      *    医療機関情報領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC         OCCURS  10.
               05  TBL-SYS1001         PIC X(610).
           03  TBL-SYS1001-MAX         PIC 9(02).
      *
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-PTNUM         PIC X(20).
               05  KEY-N-SRYYM         PIC 9(06).
               05  KEY-N-NYUGAIKBN     PIC X(01).
               05  KEY-N-RECEKBN       PIC X(01).
           03  KEY-OLD.
               05  KEY-O-PTNUM         PIC X(20).
               05  KEY-O-SRYYM         PIC 9(06).
               05  KEY-O-NYUGAIKBN     PIC X(01).
               05  KEY-O-RECEKBN       PIC X(01).
      *
       01  WRK-AREA.
           COPY    "CPORCSPRTLNK.INC".
           05  WRK-PARA-NYUGAIKBN      PIC X(01).
           05  WRK-PARA-JOBID          PIC 9(07).
           05  WRK-PARA-SHELLID        PIC X(08).
           05  WRK-PARA-HOSPNUM        PIC 9(02).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
           03  WRK-RECENUM             PIC 9(02).
           03  WRK-PTNUM-RENNUM        PIC 9(02).
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-MCP-RC              PIC 9(9).
           03  WRK-ERR-FILE-STS        PIC X(02).
           03  WRK-ERR-FILE-MSG        PIC X(100).
           03  WRK-ERR-FILE-MSG1       PIC X(17).
           03  WRK-ERR-FLG             PIC 9(01).
      *    エラーファイル名称領域
           COPY    "CPTEMPFL.INC"      REPLACING  //TEMPFLPARA//
                                       BY         //WRK-ERR-FILE-NM//.
      *
       01  WRK-SGETTEMP-ST             PIC 9(05).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関情報
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
      *
           COPY    "CPRCF010.INC".
      *
       01  JIBAI-PRT-REC.
           COPY    "CPJIBAI-PRT.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE                   DIVISION
                                   USING   COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              CNT-AREA
                                   WRK-AREA
                                   FLG-AREA
                                   KEY-AREA
                                   SPA-AREA
                                   RECEERR
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
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
      *
           DISPLAY "WRK-PARA-HOSPNUM     =" WRK-PARA-HOSPNUM
           DISPLAY "WRK-PARA-NYUGAIKBN   =" WRK-PARA-NYUGAIKBN
           DISPLAY "LNK-PRTKANRI-SRYYM   =" LNK-PRTKANRI-SRYYM
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    WRK-PARA-HOSPNUM    TO  JIBAIPARA-HOSPNUM
           MOVE    "JIBAI"             TO  JIBAIPARA-FORM-ID
           MOVE    LNK-PRTKANRI-TERMID TO  JIBAIPARA-TERMID
           MOVE    SMCNDATE-YMD        TO  JIBAIPARA-YMD
           MOVE    SMCNDATE-TIME       TO  JIBAIPARA-TIME
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    JIBAIPARA           TO  SGETTEMP-BASENAMES  (2)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  ASGNPARA
           MOVE   SGETTEMP-ST          TO  ASGNPARA-ST
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBM400"      TO  JOB-PGID
           MOVE    "自賠責ＰＤＦファイル作成"
                                   TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    医療機関情報取得
           MOVE     SPACE              TO  TBL-SYS1001-T
           INITIALIZE                      TBL-SYS1001-T
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1001"              TO  SYS-KANRICD
           MOVE    "*"                 TO  SYS-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-N-SEC
               PERFORM UNTIL ( FLG-SYSKANRI    =   1  )
                       OR    ( TBL-SYS1001-MAX >=  10 )
                   MOVE    MCPDATA-REC         TO  SYS-1001-REC
                   ADD     1             TO  TBL-SYS1001-MAX
                   MOVE    SYS-1001-REC  TO  TBL-SYS1001
                                                   (TBL-SYS1001-MAX)
      *
                   MOVE    "tbl_syskanri"    TO  MCP-TABLE
                   MOVE    "key8"            TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-N-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      TBL-SYS1001-MAX     =   ZERO
               MOVE    "医療機関情報が取得できませんでした。"
                                         TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           OPEN    OUTPUT              PRT-FILE
      *
           MOVE    "key87"             TO  WRK-MCP-PATHNAME
           MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
           PERFORM 900-SEIKYU-SELECT-SEC
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO            TO  WRK-RECENUM
      *
           MOVE    KEY-NEW         TO  KEY-OLD
           PERFORM     UNTIL   FLG-END     =   1
                       OR      KEY-NEW NOT =   KEY-OLD
      *
               MOVE    SPACE               TO  JIBAI-PRT-REC
               INITIALIZE                      JIBAI-PRT-REC
               MOVE    RECE10-KEY          TO  JIBAI-PRT-KEY1
               MOVE    JIBAI-PRT-REC       TO  MCPDATA-REC
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "tbl_jibai_prt"     TO  MCP-TABLE
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "key"               TO  MCP-PATHNAME
                   MOVE    "tbl_jibai_prt"     TO  MCP-TABLE
                   PERFORM 900-JIBAI-PRT-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-JIBAI-PRT
               END-IF
      *
               IF      FLG-JIBAI-PRT       =   1
                   STRING  "該当患者のＰＤＦデータが存在しません"
                                               DELIMITED  BY  SIZE
                           "　患者番号＝"      DELIMITED  BY  SIZE
                           RECE10-PTNUM        DELIMITED  BY  SIZE
                                               INTO  WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
      *
               ADD     1                   TO  WRK-RECENUM
               PERFORM         UNTIL   FLG-JIBAI-PRT =   1
                   MOVE    SPACE               TO  PRT-REC
      *
      *            医療機関情報
                   MOVE    ZERO                TO  FLG-SET
      *
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >       TBL-SYS1001-MAX
                           OR      FLG-SET =       1
                       MOVE    TBL-SYS1001 (IDX)   TO  SYS-1001-REC
                       IF    ( SYS-1001-STYUKYMD (1:6)
                                               <=  JIBAI-PRT-SRYYM     )
                       AND   ( JIBAI-PRT-SRYYM <=
                                               SYS-1001-EDYUKYMD (1:6) )
                           MOVE     1                  TO  FLG-SET
                       END-IF
                   END-PERFORM
                   IF      FLG-SET             =   ZERO
                       STRING  "医療機関情報が取得できませんでした。"
                               "診療年月＝"    DELIMITED  BY  SIZE
                               JIBAI-PRT-SRYYM DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                       END-STRING
                       PERFORM 500-ERR-HENSYU-SEC
                       PERFORM 500-COBABORT-SEC
                   END-IF
                   MOVE    SYS-1001-HOSPCD     TO  PRT-HOSPCD
      *
                   EVALUATE    JIBAI-PRT-NYUGAIKBN
                       WHEN    "1"
                           MOVE    "0"             TO  PRT-NYUGAIKBN
                       WHEN    "2"
                           MOVE    "1"             TO  PRT-NYUGAIKBN
                   END-EVALUATE
                   MOVE    JIBAI-PRT-SRYYM     TO  PRT-SRYYM
                   MOVE    JIBAI-PRT-PRTKBN    TO  PRT-PRTKBN
                   MOVE    "orca"              TO  PRT-MAKERCD
                   MOVE    "2"                 TO  PRT-PTNUM-KBN
                   MOVE    RECE10-PTNUM        TO  PRT-PTNUM
                   MOVE    WRK-RECENUM         TO  PRT-RECENUM
                   MOVE    JIBAI-PRT-RENNUM    TO  PRT-RENNUM
                   MOVE    JIBAI-PRT-FORMID    TO  PRT-PRTID
      *            枠無しを枠有りに変更
                   EVALUATE    JIBAI-PRT-FORMID
                       WHEN    "HCM23V06.red"
                           MOVE    "HCM48V02.red"  TO  PRT-PRTID
                       WHEN    "HCM53V02.red"
                           MOVE    "HCM54V02.red"  TO  PRT-PRTID
                       WHEN    "HCN23V08.red"
                           MOVE    "HCN48V04.red"  TO  PRT-PRTID
                       WHEN    "HCN53V04.red"
                           MOVE    "HCN54V04.red"  TO  PRT-PRTID
      *                    続紙
                       WHEN    "HCM29V04.red"
                           MOVE    "HCM49V02.red"  TO  PRT-PRTID
                   END-EVALUATE
                   MOVE    JIBAI-PRT-PRTDATA   TO  PRT-PRTDATA
                   WRITE   PRT-REC
      *
                   IF      STS-PRT             =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-RECEERR
                       STRING "ファイル 書き込みエラー  STS="
                                                   DELIMITED  BY  SIZE
                               STS-PRT             DELIMITED  BY  SIZE
                                           INTO    WRK-RECEERR
                       END-STRING
                       PERFORM 500-ERR-HENSYU-SEC
                       PERFORM 500-COBABORT-SEC
                   END-IF
      *
                   MOVE    "key"               TO  MCP-PATHNAME
                   MOVE    "tbl_jibai_prt"     TO  MCP-TABLE
                   PERFORM 900-JIBAI-PRT-READ-SEC
               END-PERFORM
      *
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "tbl_jibai_prt"     TO  MCP-TABLE
               PERFORM 900-CLOSE-SEC
      *
               ADD     1                   TO  CNT-OUT
      *
               PERFORM 900-SEIKYU-READ-SEC
           END-PERFORM
           .
       2001-JIBAI-PRT-HENSYU-EXT.
           EXIT.

      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE                       PRT-FILE
      *
           IF      CNT-OUT             >   ZERO 
               MOVE    SPACE           TO  SHELLTBL
               MOVE    "upload_jibai_receipt.sh"
                                       TO  SHELLTBL-NAME
               MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG1
               MOVE    LNK-PRTKANRI-SRYYM
                                       TO  SHELLTBL-ARG2
               MOVE    ASGNPARA (ASGNPARA-ST:)
                                       TO  SHELLTBL-ARG3
               MOVE    SHELLTBL        TO  MCPDATA-REC
               MOVE    "SHELL"         TO  MCP-FUNC
               MOVE    "shell"         TO  MCP-TABLE
               MOVE    "monthly"       TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
           ELSE
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-OUT         TO  JOB-UPDCNT2
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
           DISPLAY "RECE300  IN   " CNT-RECE10
           DISPLAY "RECE300  OUT  " CNT-OUT
           DISPLAY "*** ORCBM400 END ***"
           .
       300-TERM-EXT.
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
           STRING  "ORCBM400 "         DELIMITED  BY  SIZE
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
      *    ファイルエラー処理
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "ファイルＯＰＥＮエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "ファイル読み込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "ファイル書き込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "ファイル更新エラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file rewrite err"
                                           TO  WRK-ERR-FILE-MSG1
           END-EVALUATE
      *
           STRING  WRK-ERR-FILE-MSG        DELIMITED   BY  SPACE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   " STS="                 DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
           END-STRING
           PERFORM 500-ERR-HENSYU-SEC
      *
           STRING  "ORCBM400 "             DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-FILE-ERR-ABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込
      *****************************************************************
       900-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-N-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-END
      *     
           MOVE    SPACE               TO  RECE10-REC
           INITIALIZE                      RECE10-REC
           MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECE10-SKYYM
           EVALUATE    WRK-PARA-NYUGAIKBN
               WHEN    ZERO
                   MOVE    "%"                 TO  RECE10-NYUGAIKBN
               WHEN    ANY
                   MOVE    WRK-PARA-NYUGAIKBN  TO  RECE10-NYUGAIKBN
           END-EVALUATE
           MOVE    4                   TO  RECE10-TEISYUTUSAKI
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
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
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-READ-SEC             SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-END
               MOVE    MCPDATA-REC         TO  RECE10-REC
               MOVE    RECE10-PTNUM        TO  KEY-N-PTNUM
               MOVE    RECE10-NYUGAIKBN    TO  KEY-N-NYUGAIKBN
               MOVE    RECE10-SRYYM        TO  KEY-N-SRYYM
               MOVE    RECE10-RECEKBN      TO  KEY-N-RECEKBN
      *
               DISPLAY "PTNUM=" RECE10-PTNUM " " RECE10-SRYYM " "
                       RECE10-NYUGAIKBN " " RECE10-RECEKBN
               ADD     1                   TO  CNT-RECE10
           ELSE
               MOVE    HIGH-VALUE          TO  KEY-NEW
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責ＰＤＦ読込
      *****************************************************************
       900-JIBAI-PRT-READ-SEC           SECTION.
      *
           MOVE    "tbl_jibai_prt"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-JIBAI-PRT
               MOVE    MCPDATA-REC         TO  JIBAI-PRT-REC
           ELSE
               MOVE    1                   TO  FLG-JIBAI-PRT
           END-IF
      *
           .
       900-JIBAI-PRT-READ-EXT.
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
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ読み込みエラー　TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCR0700 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               CALL    "cobabort"          USING   WRK-ERRMSG
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
