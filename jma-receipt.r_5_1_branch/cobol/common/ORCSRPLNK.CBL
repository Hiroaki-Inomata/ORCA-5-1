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
       PROGRAM-ID.             ORCSRPLNK.
      *****************************************************************
      *  システム名        : ORCA
      *  サブシステム名    : 共通
      *  コンポーネント名  : オンライン帳票名保存
      *  作成日付    作業者        記述
      *  19/09/03    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  FLG-AREA.
           03  FLG-APIPARA         PIC 9(01).
      *
       01  WRK-AREA.
           03  WRK-EDANUM          PIC 9(02).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
       COPY    "CPORCSACCESS.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "CPORCSRPLNK.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           SRPLNK-AREA
           SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE              FLG-AREA
                                   WRK-AREA
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
           MOVE    ZERO            TO  SRPLNK-RC
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
      D    DISPLAY "SRPLNK-MODE = " SRPLNK-MODE
      D    DISPLAY "SPA-PRT-UUID= " SPA-PRT-UUID
      D    DISPLAY "SPA-OPID    = " SPA-OPID
      *
           EVALUATE    SRPLNK-MODE
           WHEN    "CHK_MONBLOB"
               INITIALIZE              SACCESS-AREA
               MOVE    "/usr/lib/panda/MONBLOB.so"
                                   TO  SACCESS-IN-FILENAME
               CALL    "ORCSACCESS"    USING
                                       SACCESS-AREA
      *
               MOVE    SACCESS-RC  TO  SRPLNK-RC
           WHEN    "INS"
               IF    ( SPA-PRT-UUID    NOT =   SPACE )
                AND  ( SPA-PRT-FLG     NOT =   SPACE )
                AND  ( SRPLNK-VAL      NOT =   SPACE )
      *
                   INITIALIZE                  APIPARA-REC
                   MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
                   MOVE    "RPLNK"         TO  APIPARA-GYOUMUID
                   MOVE    SPA-PRT-UUID    TO  APIPARA-TERMID
                   MOVE    SPA-OPID        TO  APIPARA-FILEMEI
                   PERFORM 800-EDANUM-SET-SEC
                   MOVE    WRK-EDANUM      TO  APIPARA-EDANUM
                   MOVE    SRPLNK-AREA     TO  APIPARA-DATA-REC
                   MOVE    APIPARA-REC     TO  MCPDATA-REC
                   MOVE    "tbl_api_para"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBDELETE-SEC
      *
                   MOVE    APIPARA-REC     TO  MCPDATA-REC
                   MOVE    "tbl_api_para"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
      *
               END-IF
      *
           WHEN    "UPD"
      *
                   INITIALIZE                  APIPARA-REC
                   MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
                   MOVE    "RPLNK"         TO  APIPARA-GYOUMUID
                   MOVE    SPA-PRT-UUID    TO  APIPARA-TERMID
                   MOVE    SPA-OPID        TO  APIPARA-FILEMEI
                   PERFORM 800-EDANUM-SET-SEC
                   MOVE    WRK-EDANUM      TO  APIPARA-EDANUM
                   MOVE    SRPLNK-AREA     TO  APIPARA-DATA-REC
                   MOVE    APIPARA-REC     TO  MCPDATA-REC
                   MOVE    "tbl_api_para"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
      *
           WHEN    "SEL"
      *
               MOVE    SPACE               TO  SRPLNK-VAL
      *
               PERFORM 900-APIPARA-KEY-SEL-SEC
               IF    ( FLG-APIPARA     =   ZERO )
                   MOVE    APIPARA-DATA-REC
                                           TO  SRPLNK-AREA
      *
               ELSE
                   INITIALIZE              SRPLNK-AREA
                   STRING  "unknown_"      DELIMITED   BY  SIZE
                           SPA-PRT-FLG     DELIMITED   BY  SIZE
                   INTO    SRPLNK-VAL
                   END-STRING
               END-IF
           WHEN    "DEL_GROUP"
               INITIALIZE                  APIPARA-REC
               MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
               MOVE    "RPLNK"         TO  APIPARA-GYOUMUID
               MOVE    SPA-PRT-UUID    TO  APIPARA-TERMID
               MOVE    SPA-OPID        TO  APIPARA-FILEMEI
               MOVE    APIPARA-REC     TO  MCPDATA-REC
               MOVE    "tbl_api_para"  TO  MCP-TABLE
               MOVE    "del1"          TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
      *
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    枝番号設定処理
      *****************************************************************
       800-EDANUM-SET-SEC              SECTION.
      *
           MOVE    ZERO            TO  WRK-EDANUM
      *
           IF    ( FUNCTION TRIM(SPA-PRT-FLG) IS  NUMERIC )
               MOVE    FUNCTION NUMVAL (
                           FUNCTION TRIM(SPA-PRT-FLG)
                       )           TO  WRK-EDANUM
           END-IF
      *
           .
       800-EDANUM-SET-EXT.
           EXIT.
      *****************************************************************
      *     ＡＰＩパラメタ検索処理
      *****************************************************************
       900-APIPARA-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-APIPARA
      *
           INITIALIZE                  APIPARA-REC
           MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
           MOVE    "RPLNK"         TO  APIPARA-GYOUMUID
           MOVE    SPA-PRT-UUID    TO  APIPARA-TERMID
           MOVE    SPA-OPID        TO  APIPARA-FILEMEI
           PERFORM 800-EDANUM-SET-SEC
           MOVE    WRK-EDANUM      TO  APIPARA-EDANUM
           MOVE    APIPARA-REC     TO  MCPDATA-REC
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC
                                   TO  APIPARA-REC
           ELSE
               MOVE    1           TO  FLG-APIPARA
           END-IF
      *
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-APIPARA-KEY-SEL-EXT.
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
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
