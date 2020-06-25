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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCGQSUB06.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者照会
      *  コンポーネント名  : 印刷・ファイル出力処理実行
      *  管理者            : 
      *  作成日付    作業者        記述
      *  19/04/16    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *01  CONST-AREA.
      *
      *    照会印刷バッチパラメタ
           COPY    "CPBQ01.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    シェルテーブル
           COPY    "CPSHELLTBL.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    パラメタ保存・取得
           COPY    "CPORCSPARA.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *
       01  UIDIO-AREA.
           03  UIDIO-RET               PIC X(2).
           03  UIDIO-UID               PIC X(36).
       01  UIDIO-ST                    PIC 9(2).
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPQSUB06.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           QSUB06-LNK
           SPA-AREA.
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
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2001-PRINT-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    印刷シェル実行
      *****************************************************************
       2001-PRINT-SEC                  SECTION.
      *
           INITIALIZE                      BQ01
           MOVE    ZERO                TO  BQ01-REQUEST-G
           MOVE    QSUB06-REQUEST-CSV  TO  BQ01-REQUEST-CSV
           MOVE    QSUB06-REQUEST-PRINT
                                       TO  BQ01-REQUEST-PRINT
      *
           MOVE    QSUB06-CSVFILENAME  TO  BQ01-CSVFILENAME
      *
           MOVE    QSUB06-HYODAI       TO  BQ01-HYODAI
           MOVE    QSUB06-JYOKEN1      TO  BQ01-JYOKEN1
           MOVE    QSUB06-JYOKEN2      TO  BQ01-JYOKEN2
           MOVE    QSUB06-STPAGE       TO  BQ01-STPAGE
           MOVE    QSUB06-EDPAGE       TO  BQ01-EDPAGE
           MOVE    QSUB06-STKNS        TO  BQ01-STKNS
           MOVE    QSUB06-EDKNS        TO  BQ01-EDKNS
      *
           MOVE    SPA-SYSYMD          TO  BQ01-KJYNYMD
      *
           MOVE    QSUB06-SQL          TO  BQ01-SQL
      *
           MOVE    SPA-SYSYMD          TO  BQ01-SYSYMD
           MOVE    QSUB06-SELHMS       TO  BQ01-SYSTIME
           MOVE    SPA-STAFFCD         TO  BQ01-STAFFCD
           MOVE    QSUB06-SHELLID      TO  BQ01-SHELLID
           MOVE    QSUB06-FILESVKBN    TO  BQ01-FILESVKBN
           MOVE    QSUB06-DIRNAME      TO  BQ01-DIRNAME
           MOVE    QSUB06-FILENAME     TO  BQ01-FILENAME
           MOVE    QSUB06-KARTE-UID    TO  BQ01-KARTE-UID
           MOVE    QSUB06-OUTKBN       TO  BQ01-OUTKBN
      *
           INITIALIZE                      SPARA-AREA
           MOVE    "SAVE"              TO  SPARA-REQUEST
           MOVE    BQ01                TO  SPARA-DATA
           CALL    "ORCSPARA"              USING
                                           SPARA-AREA
                                           SPA-AREA
      *
      *    印刷用スクリプト処理
      *
           MOVE    SPACE               TO  SHELLTBL
           INITIALIZE                      SHELLTBL
      *
           MOVE    QSUB06-SHELLNAME    TO  SHELLTBL-NAME
           MOVE    QSUB06-REQUEST-G    TO  SHELLTBL-ARG1
           IF    ( QSUB06-FILESVKBN    =   "1" )
               STRING  QSUB06-DIRNAME  DELIMITED BY SPACE
                       "/"             DELIMITED BY SIZE
                       QSUB06-FILENAME DELIMITED BY SPACE
               INTO                        SHELLTBL-ARG2
               END-STRING
           ELSE
               MOVE    BQ01-CSVFILENAME
                                       TO  SHELLTBL-ARG2
           END-IF
      *
           MOVE    SPARA-ID            TO  SHELLTBL-ARG3
           IF    ( QSUB06-SHELLID      =   "API" )
               MOVE    QSUB06-ORCA-UID TO  SHELLTBL-ARG4
           ELSE
               MOVE    SPA-TERMID      TO  SHELLTBL-ARG4
           END-IF
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG5
           MOVE    QSUB06-JOBID        TO  SHELLTBL-ARG6
           MOVE    QSUB06-SHELLID      TO  SHELLTBL-ARG7
           MOVE    QSUB06-FILESVKBN    TO  SHELLTBL-ARG8
           MOVE    QSUB06-ENCODEKBN    TO  SHELLTBL-ARG9
           MOVE    QSUB06-OUTKBN       TO  SHELLTBL-ARG10
           MOVE    QSUB06-TBL-KEY      TO  SHELLTBL-ARG11
           MOVE    QSUB06-KARTE-UID    TO  SHELLTBL-ARG12
           MOVE    QSUB06-DATA-ID      TO  SHELLTBL-ARG13
           MOVE    QSUB06-PRINT-ID     TO  SHELLTBL-ARG14
           MOVE    QSUB06-PDFFILENAME  TO  SHELLTBL-ARG15
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       2001-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    UID取得処理
      *****************************************************************
       800-ORCUIDSET-SEC               SECTION.
      *
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
      *
           .
       800-ORCUIDSET-EXT.
           EXIT.
