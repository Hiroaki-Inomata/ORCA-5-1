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
       PROGRAM-ID.             ORCSMONBATCH.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : バッチジョブ制御
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/09/26    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
       01  FLG-AREA.
           03  FLG-MONBATCH            PIC 9(01).
           03  FLG-LOOPEND             PIC 9(01).
           03  FLG-RUNNING             PIC 9(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPSHELLTBL.INC".
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                        SECTION.
      *
           COPY    "CPORCSMONBATCH.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                       DIVISION    USING
                                       SMONBATCH-AREA
                                       SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           PERFORM 200-MAIN-SEC
      *    終了処理
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           MOVE    ZERO                TO  SMONBATCH-RC
                                           SMONBATCH-FOUND
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( SMONBATCH-MODE      =    "CHK" OR "DEL" )
               PERFORM 2001-MONBATCH-CHK-SEC
           ELSE
               MOVE    100             TO   SMONBATCH-RC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    実行中ジョブチェック処理
      *****************************************************************
       2001-MONBATCH-CHK-SEC               SECTION.
      *
           PERFORM 900-MONBATCH-SEL-SEC
      *
           PERFORM UNTIL ( FLG-MONBATCH    NOT =   ZERO )
                    OR   ( SMONBATCH-RC    NOT =   ZERO )
      *
               IF    ( SMONBATCH-KEY       NOT =   SPACE )
                   IF    ( SHELLTBL-SHELLID    =   SMONBATCH-SHELLID )
                     AND(( SMONBATCH-OPID      =   SPACE )
                       OR( SHELLTBL-RUN-OPID   =   SMONBATCH-OPID ))
                     AND(( SMONBATCH-TERMID    =   SPACE )
                       OR( SHELLTBL-TERMID     =   SMONBATCH-TERMID ))
      *
                       MOVE    1               TO  SMONBATCH-FOUND
      *
                       IF    ( SMONBATCH-MODE  =   "DEL" )
                           MOVE    SHELLTBL    TO  MCPDATA-REC
                           MOVE    "shell"     TO  MCP-TABLE
                           MOVE    LOW-VALUE   TO  MCP-PATHNAME
                           PERFORM 910-DBDELETE-SEC
                           MOVE    MCP-RC      TO  SMONBATCH-RC
                       END-IF
      *
                   END-IF
               END-IF
      *
               PERFORM 900-MONBATCH-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-MONBATCH-CHK-EXT.
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
      *
      *****************************************************************
      *    MONBATCH検索処理
      *****************************************************************
       900-MONBATCH-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-MONBATCH
      *
           INITIALIZE                      SHELLTBL
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-HOSPNUM
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-MONBATCH-FET-SEC
           ELSE
               MOVE    1               TO  FLG-MONBATCH
           END-IF
      *
           .
       900-MONBATCH-SEL-EXT.
           EXIT.
      *****************************************************************
      *    MONBATCH FETCH処理
      *****************************************************************
       900-MONBATCH-FET-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-MONBATCH
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-MONBATCH    NOT =   ZERO )
                    OR   ( FLG-LOOPEND     NOT =   ZERO )
      *
               MOVE    "shell"         TO  MCP-TABLE
               MOVE    LOW-VALUE       TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
      *
                   MOVE    MCPDATA-REC TO  SHELLTBL
      *
                   MOVE    1           TO  FLG-LOOPEND
      *
                   IF    ( SHELLTBL-HOSPNUM    NOT =  SPA-HOSPNUM )
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   MOVE    1           TO  FLG-MONBATCH
               END-IF
      *
           END-PERFORM
      *
           .
       900-MONBATCH-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
                                   MCPAREA
                                   MCPDATA-REC
                                   SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
                                   MCPAREA
                                   MCPDATA-REC
                                   SPA-AREA
      *
           .
      *
       911-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
                                   MCPAREA
                                   MCPDATA-REC
                                   SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
           .
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
                                   MCPAREA
                                   MCPDATA-REC
                                   SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
           .
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
                                   MCPAREA
                                   MCPDATA-REC
                                   SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC            SECTION.
      *
           MOVE    "DBDELETE"  TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
                                   MCPAREA
                                   MCPDATA-REC
                                   SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
