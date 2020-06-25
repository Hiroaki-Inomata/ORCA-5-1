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
       PROGRAM-ID.             ORCSUPDTOUKEIPRM.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 統計パラメタ
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
           03  FLG-SYSKANRI            PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-KANRICD             PIC X(04).
           03  WRK-PGID                PIC X(20).
      *
       01  TSYS-AREA.
           03  TSYS-MAX                PIC 9(05).
           03  TSYS-IDX                PIC 9(05).
           03  TSYS-OCC                OCCURS  200.
               05  TSYS-PGID           PIC X(20).
               05  TSYS-KANRICD        PIC X(04).
               05  TSYS-KBNCD          PIC X(08).
      *
       01  CONST-AREA.
           03  CONST-TSYS-MAX          PIC 9(05)   VALUE   200.
           03  CONST-SYS0043           PIC X(04)   VALUE   "0043".
           03  CONST-SYS0044           PIC X(04)   VALUE   "0044".
           03  CONST-SYS3001           PIC X(04)   VALUE   "3001".
           03  CONST-SYS3002           PIC X(04)   VALUE   "3002".
      *
      *    日次パラメタ一時ファイル領域
           COPY    "CPSK3001.INC"      REPLACING   //SYS-//
                                       BY          //TMP-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *    日次パラメタファイル(雛形) ※月次用にも使用
           COPY    "CPSK0043.INC".
      *    日次パラメタファイル       ※月次用にも使用
           COPY    "CPSK3001.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                        SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                       DIVISION    USING
                                       SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
      *
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
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
                                       TSYS-AREA
      *
           MOVE    CONST-SYS0043   TO  WRK-KANRICD
           PERFORM 1001-EDIT-TSYS-SEC
      *
           MOVE    CONST-SYS0044   TO  WRK-KANRICD
           PERFORM 1001-EDIT-TSYS-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理退避領域編集処理
      *****************************************************************
       1001-EDIT-TSYS-SEC              SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    WRK-KANRICD     TO  SYS-KANRICD
           MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    SYSKANRI-REC        TO  SYS-0043-REC
               MOVE    SYS-0043-PRGNAME    TO  WRK-PGID
               PERFORM 10011-EDIT-TSYS-SEC
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-EDIT-TSYS-EXT.
           EXIT.
      *****************************************************************
      *    システム管理退避領域編集処理
      *****************************************************************
       10011-EDIT-TSYS-SEC             SECTION.
      *
           IF  ( CONST-TSYS-MAX    >   TSYS-MAX )
               COMPUTE TSYS-MAX    =   TSYS-MAX    +   1
               MOVE    SYS-KANRICD TO  TSYS-KANRICD (TSYS-MAX)
               MOVE    SYS-KBNCD   TO  TSYS-KBNCD   (TSYS-MAX)
               MOVE    WRK-PGID    TO  TSYS-PGID    (TSYS-MAX)
           END-IF
      *
           .
       10011-EDIT-TSYS-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    CONST-SYS3001   TO  WRK-KANRICD
           PERFORM 2001-UPDATE-TOUKEIPRM-SEC
      *
           MOVE    CONST-SYS3002   TO  WRK-KANRICD
           PERFORM 2001-UPDATE-TOUKEIPRM-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計パラメタ情報更新処理
      *****************************************************************
       2001-UPDATE-TOUKEIPRM-SEC       SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    WRK-KANRICD     TO  SYS-KANRICD
           MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
      *
               MOVE    SYSKANRI-REC        TO  SYS-3001-REC
               MOVE    SYS-3001-PRGNAME    TO  WRK-PGID
      *
               PERFORM VARYING TSYS-IDX    FROM    1   BY  1
                       UNTIL ( TSYS-IDX    >   TSYS-MAX )
      *
                   IF    ( TSYS-PGID (TSYS-IDX)
                                           =   WRK-PGID )
                       INITIALIZE              SYSKANRI-REC
                       MOVE    SPA-HOSPNUM TO  SYS-HOSPNUM
                       MOVE    TSYS-KANRICD (TSYS-IDX)
                                           TO  SYS-KANRICD
                       MOVE    TSYS-KBNCD   (TSYS-IDX)
                                           TO  SYS-KBNCD
                       MOVE    SPA-SYSYMD  TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                       PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                       MOVE    SYSKANRI-REC    TO  TMP-3001-REC
                       IF    ( FLG-SYSKANRI    =   ZERO )
                           IF    ( SYS-3001-PARA-AREA
                                               =   TMP-3001-PARA-AREA )
                               CONTINUE
                           ELSE
                               MOVE    TMP-3001-PARA-AREA
                                               TO  SYS-3001-PARA-AREA
                               MOVE    SYS-3001-REC    TO  MCPDATA-REC
                               MOVE    "tbl_syskanri"  TO  MCP-TABLE
                               MOVE    "key"           TO  MCP-PATHNAME
                               PERFORM 910-DBUPDATE-SEC
                               DISPLAY "UPDATED " SYS-3001-KANRICD
                                       "/"        SYS-3001-KBNCD
                                       "/"        WRK-PGID
                           END-IF
      *
                           INITIALIZE                  SYSKANRI-REC
                           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
                           IF    ( SYS-3001-KANRICD = "3001" )
                               MOVE    "3101"      TO  SYS-KANRICD
                           ELSE
                               MOVE    "3102"      TO  SYS-KANRICD
                           END-IF
                           MOVE    SYS-3001-KBNCD  TO  SYS-KBNCD
                           MOVE    SYS-3001-STYUKYMD
                                                   TO  SYS-STYUKYMD
                           MOVE    SYS-3001-EDYUKYMD
                                                   TO  SYS-EDYUKYMD
                           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
                           MOVE    "tbl_syskanri"  TO  MCP-TABLE
                           MOVE    "key"           TO  MCP-PATHNAME
                           PERFORM 910-DBDELETE-SEC
      *
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-UPDATE-TOUKEIPRM-EXT.
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
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC   SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC   SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
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
