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
       PROGRAM-ID.             ORCSAPIFMTERM.
      *****************************************************************
      *  システム名        : ORCA
      *  サブシステム名    : 共通
      *  コンポーネント名  : API用帳票データ作成後処理
      *  作成日付    作業者        記述
      *  09/02/12    NACL-太田     新規作成
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
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
       01  FLG-AREA.
           03  FLG-APIPARA             PIC 9(01).
      *
       01  IDX-AREA.
           03  IDXF                    PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TEXT                PIC X(32).
      *
       01  CONST-AREA.
           03  CONST-FORM-MAX          PIC 9(03)   VALUE 10.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    オンライン帳票ＩＤ連絡
           COPY    "CPORCSRPLNK.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    帳票データPUSH通信用
           COPY    "CPPUSHPRINT001.INC".
      *
      *    ＡＰＩパラメタ
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
           COPY    "CPPARA001.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
            COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF    ( SPA-API-FLG     =   1 OR 2 )
               IF    ( SPA-PRTCONF =   "2" OR "3" )
                   CONTINUE
               ELSE
                   PERFORM 2001-PUSH-SEC
               END-IF
           END-IF
      *
           PERFORM 2001-APIPARA-DEL-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    PUSH処理
      *****************************************************************
       2001-PUSH-SEC                   SECTION.
      *
           MOVE    ZERO                TO  IDXF
      *
           INITIALIZE                      PUSHP001-REC
      *
           MOVE    "print001"          TO  PUSHP001-EVENT
           MOVE    SPA-OPID            TO  PUSHP001-OPID
      *
           PERFORM 900-APIPARA-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-APIPARA NOT =   ZERO )
                    OR   ( IDXF           >=  CONST-FORM-MAX )
      *
               COMPUTE IDXF  = IDXF  + 1
      *
               MOVE    APIPARA-DATA-REC TO PARA001-REC
      *
               MOVE    PARA001-FORMID      TO  PUSHP001-FORMID  (IDXF)
               MOVE    PARA001-CUSTOMID    TO  PUSHP001-CUSTOMID(IDXF)
               EVALUATE    TRUE
               WHEN    ( PARA001-FORMNAME (1:8)    =    "支払日別" )
                   MOVE    "支払証明書（日別）"
                                           TO  PUSHP001-FORMNAME (IDXF)
               WHEN    ( PARA001-FORMNAME (1:8)    =    "支払月別" )
                   MOVE    "支払証明書（月別）"
                                           TO  PUSHP001-FORMNAME (IDXF)
               WHEN    ( PARA001-FORMNAME (1:8)    =    "支払証明" )
                   MOVE    "支払証明書"    TO  PUSHP001-FORMNAME (IDXF)
               WHEN    OTHER
                   MOVE  PARA001-FORMNAME  TO  PUSHP001-FORMNAME(IDXF)
               END-EVALUATE
               MOVE    PARA001-DATAID      TO  PUSHP001-DATAID  (IDXF)
      *
               PERFORM 900-APIPARA-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( IDXF            >   ZERO )
               MOVE    "push_print001"     TO  MCP-TABLE
               MOVE    LOW-VALUE           TO  MCP-PATHNAME
               MOVE    "PUSHEVENT"         TO  MCP-FUNC
               CALL    "MONFUNC"               USING
                                               MCPAREA
                                               PUSHP001-REC
           END-IF
      *
           .
       2001-PUSH-EXT.
           EXIT.
      *****************************************************************
      *    APIPARA削除処理
      *****************************************************************
       2001-APIPARA-DEL-SEC            SECTION.
      *
           INITIALIZE                  APIPARA-REC
           MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
           MOVE    "api001"        TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID      TO  APIPARA-TERMID
           MOVE    SPA-OPID        TO  APIPARA-FILEMEI
           MOVE    APIPARA-REC     TO  MCPDATA-REC
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "del1"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           IF    ( SPA-PRT-UUID    NOT =   SPACE )
               INITIALIZE                  SRPLNK-AREA
               MOVE    "DEL_GROUP"     TO  SRPLNK-MODE
               CALL    "ORCSRPLNK"         USING
                                           SRPLNK-AREA
                                           SPA-AREA
           END-IF
      *
           .
       2001-APIPARA-DEL-EXT.
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
      *    APIパラメタ検索処理
      *****************************************************************
       900-APIPARA-KEY2-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-APIPARA
      *
           INITIALIZE                  APIPARA-REC
      *
           MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
           MOVE    "api001"        TO  APIPARA-GYOUMUID
           MOVE    SPA-TERMID      TO  APIPARA-TERMID
           MOVE    SPA-OPID        TO  APIPARA-FILEMEI
           MOVE    APIPARA-REC     TO MCPDATA-REC
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  APIPARA-REC
               MOVE    ZERO        TO  FLG-APIPARA
           ELSE
               MOVE    1           TO  FLG-APIPARA
               INITIALIZE              APIPARA-REC
           END-IF
      *
           .
       900-APIPARA-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    APIパラメタFETCH処理
      *****************************************************************
       900-APIPARA-KEY2-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-APIPARA
      *
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  APIPARA-REC
               MOVE    ZERO        TO  FLG-APIPARA
           ELSE
               MOVE    1           TO  FLG-APIPARA
               INITIALIZE              APIPARA-REC
           END-IF
      *
           .
       900-APIPARA-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
