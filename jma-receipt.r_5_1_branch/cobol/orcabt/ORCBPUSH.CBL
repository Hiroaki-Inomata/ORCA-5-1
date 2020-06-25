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
       PROGRAM-ID.             ORCBPUSH.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : ＰＵＳＨ通知処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  18/02/15    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
       COPY    "COMMON-SPA".
      *
      *    エラーファイル 名称領域 
           COPY    "CPRECEERR.INC".
      *
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
      *
       01  INDEX-AREA.
           03  IDW                     PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           COPY    "CPORCSPRTLNK.INC".
           05  WRK-PARA-JOBID          PIC 9(07).
           05  WRK-PARA-SHELLID        PIC X(08).
           05  WRK-PARA-HOSPNUM        PIC 9(02).
           05  WRK-PARA-EVENT          PIC X(50).
           05  WRK-PARA-DATA-OCC           OCCURS 10.
               07  WRK-PARA-DATA       PIC X(10).
      *
           03  WRK-CNT                 PIC 9(10).
           03  WRK-CNT-X   REDEFINES   WRK-CNT
                                       PIC X(10).
      *
           03  WRK-NUM-X               PIC X(10).
           03  WRK-NUM     REDEFINES   WRK-NUM-X
                                       PIC 9(10).
      *
           03  WRK-RECEERR             PIC X(200).
      *
      *    日付変換用
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    PUSH
           COPY    "CPPUSHBATCH01.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER          PIC X(256).
      ****************************************************************
       PROCEDURE                   DIVISION
                                   USING
                                   COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
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
           INITIALIZE              WRK-AREA
                                   FLG-AREA
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
                                               WRK-PARA-HOSPNUM
                                               RECEERR
                                               WRK-PARA-EVENT
                                               WRK-PARA-DATA (1)
                                               WRK-PARA-DATA (2)
                                               WRK-PARA-DATA (3)
                                               WRK-PARA-DATA (4)
                                               WRK-PARA-DATA (5)
                                               WRK-PARA-DATA (6)
                                               WRK-PARA-DATA (7)
                                               WRK-PARA-DATA (8)
                                               WRK-PARA-DATA (9)
                                               WRK-PARA-DATA (10)
           END-UNSTRING
      *
           DISPLAY "WRK-PARA-HOSPNUM   =" WRK-PARA-HOSPNUM
           DISPLAY "WRK-PARA-EVENT     =" WRK-PARA-EVENT
           DISPLAY "LNK-PRTKANRI-SRYYM =" LNK-PRTKANRI-SRYYM
           DISPLAY "LNK-PRTKANRI-SKYYMD=" LNK-PRTKANRI-SKYYMD
           DISPLAY "WRK-PARA-DATA (1)  =" WRK-PARA-DATA (1)
           DISPLAY "WRK-PARA-DATA (2)  =" WRK-PARA-DATA (2)
           DISPLAY "WRK-PARA-DATA (3)  =" WRK-PARA-DATA (3)
           DISPLAY "WRK-PARA-DATA (4)  =" WRK-PARA-DATA (4)
           DISPLAY "WRK-PARA-DATA (5)  =" WRK-PARA-DATA (5)
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBPUSH"      TO  JOB-PGID
           MOVE    "ＰＵＳＨ通知処理"
                                   TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           EVALUATE    WRK-PARA-EVENT
               WHEN    "temp_medical_add"
               WHEN    "data_check"
                   MOVE    2               TO  JOB-JOBID
               WHEN    "statistics_form(Daily)"
               WHEN    "statistics_form(Monthly)"
                   MOVE    WRK-PARA-DATA (1)
                                           TO  WRK-NUM-X
                   MOVE    WRK-NUM         TO  JOB-JOBID
               WHEN    OTHER
                   MOVE    1               TO  JOB-JOBID
           END-EVALUATE
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           IF    ( SJOBKANRI-RETURN
                                   =   ZERO  )
           AND   ( JOB-KARTE-UID
                               NOT =   SPACE )
               INITIALIZE                  PUSHBA01-REC
               MOVE    JOB-KARTE-UID   TO  PUSHBA01API-KARTE-UID
               MOVE    JOB-ORCA-UID    TO  PUSHBA01API-ORCA-UID
               MOVE    "000"           TO  PUSHBA01API-RESULT
               MOVE    WRK-PARA-EVENT  TO  PUSHBA01-EVENT
               EVALUATE    WRK-PARA-EVENT
                   WHEN    "receiptdata_make"
                   WHEN    "receiptdata_check_make"
                   WHEN    "receiptdata_patient_make"
                       MOVE    LNK-PRTKANRI-SRYYM
                                           TO  WRK-SYMD
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE (1:7)
                                           TO  PUSHBA01-PERFORM-MONTH
                       MOVE    JOB-UPDCNT2 TO  WRK-CNT
                       MOVE    WRK-CNT-X   TO  PUSHBA01-OUTPUT-COUNT
                   WHEN    "medicalpoints_master_make"
                   WHEN    "disease_master_make"
                   WHEN    "stock1"
                   WHEN    "patient_medical_all_make"
                       MOVE    WRK-PARA-DATA (1)
                                           TO  WRK-SYMD
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE
                                           TO  PUSHBA01-PERFORM-DATE
                       MOVE    JOB-UPDCNT  TO  WRK-CNT
                       MOVE    WRK-CNT-X   TO  PUSHBA01-OUTPUT-COUNT
                   WHEN    "temp_medical_add"
                   WHEN    "statistics_form(Daily)"
                   WHEN    "statistics_form(Monthly)"
                       MOVE    JOB-UPDCNT  TO  WRK-CNT
                       MOVE    WRK-CNT-X   TO  PUSHBA01-OUTPUT-COUNT
                   WHEN    "patient_basis_all_make"
                       MOVE    WRK-PARA-DATA (1)
                                           TO  PUSHBA01-REQUEST-MODE
                       MOVE    WRK-PARA-DATA (2)
                                           TO  WRK-SYMD
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE
                                           TO  PUSHBA01-START-DATE
                       MOVE    WRK-PARA-DATA (3)
                                           TO  WRK-SYMD
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE
                                           TO  PUSHBA01-END-DATE
                       MOVE    JOB-UPDCNT  TO  WRK-CNT
                       MOVE    WRK-CNT-X   TO  PUSHBA01-OUTPUT-COUNT
                   WHEN    "patient_disease_all_make"
                       MOVE    WRK-PARA-DATA (1)
                                           TO  WRK-SYMD (1:6)
                       MOVE    "01"        TO  WRK-SYMD (7:2)
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE (1:7)
                                           TO  PUSHBA01-PERFORM-MONTH
                       MOVE    JOB-UPDCNT  TO  WRK-CNT
                       MOVE    WRK-CNT-X   TO  PUSHBA01-OUTPUT-COUNT
                   WHEN    "address_master_make"
                       MOVE    JOB-UPDCNT  TO  WRK-CNT
                       MOVE    WRK-CNT-X   TO  PUSHBA01-OUTPUT-COUNT
                   WHEN    "reprint"
                       MOVE    JOB-UPDCNT  TO  WRK-CNT
                       MOVE    WRK-CNT-X   TO  PUSHBA01-NUMBER-OF-SHEETS
                   WHEN    "receipt_print"
                       IF      WRK-PARA-DATA (1)
                                           =   "2"
                           MOVE    "9999-12"   TO
                                               PUSHBA01-PERFORM-MONTH
                       ELSE
                           MOVE    WRK-PARA-DATA (2)
                                               TO  WRK-SYMD (1:6)
                           MOVE    "01"        TO  WRK-SYMD (7:2)
                           PERFORM 801-DAYHEN01-SEC
                           MOVE    WRK-HEN-DATE (1:7)
                                               TO
                                               PUSHBA01-PERFORM-MONTH
                       END-IF
                       MOVE    JOB-UPDCNT  TO  WRK-CNT
                       MOVE    WRK-CNT-X   TO  PUSHBA01-OUTPUT-COUNT
                   WHEN    "data_check"
                       MOVE    LNK-PRTKANRI-SRYYM
                                           TO  WRK-SYMD
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE (1:7)
                                           TO  PUSHBA01-PERFORM-MONTH
                       MOVE    JOB-UPDCNT  TO  WRK-CNT
                       MOVE    WRK-CNT-X   TO  PUSHBA01-NUMBER-OF-SHEETS
      *
                       MOVE    "CHK"       TO  SJOBKANRI-MODE
                       INITIALIZE              JOBKANRI-REC
                       MOVE    1           TO  JOB-JOBID
                       MOVE    WRK-PARA-SHELLID
                                           TO  JOB-SHELLID
                       MOVE    SPA-HOSPNUM TO  JOB-HOSPNUM
                       CALL    "ORCSJOB"       USING
                                               ORCSJOBKANRIAREA
                                               JOBKANRI-REC
                                               SPA-AREA
                       IF    ( SJOBKANRI-RETURN
                                            =   ZERO  )
                       AND   ( JOB-KARTE-UID
                                        NOT =   SPACE )
                           MOVE    JOB-UPDCNT1
                                        TO  WRK-CNT
                           MOVE    WRK-CNT-X
                                        TO  PUSHBA01-NUMBER-OF-RECORDS
                       END-IF
      *
               END-EVALUATE
      *??
             display "push=" PUSHBA01-REC "#"
      *??
               MOVE    PUSHBA01-REC    TO  MCPDATA-REC
               MOVE    "PUSHEVENT"     TO  MCP-FUNC
               MOVE    "push_batch01"  TO  MCP-TABLE
               MOVE    "push_batch01"  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           ELSE
               DISPLAY "NOT FOUND JOBKANRI"
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCBPUSH END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYY             TO  WRK-HEN-YY
               MOVE    WRK-SMM             TO  WRK-HEN-MM
               MOVE    WRK-SDD             TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
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
