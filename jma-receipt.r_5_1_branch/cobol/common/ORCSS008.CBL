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
       PROGRAM-ID.             ORCSS008.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 医療区分取得(2,3のみ)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/08/21    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
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
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-RYOCOM                      PIC 9(01).
           03  FLG-NYUINACCT                   PIC 9(01).
           03  FLG-END                         PIC 9(01).
           03  FLG-NYUINDAY-G.
               05  FLG-NYUINDAY                PIC 9(01)
                                               OCCURS  31.
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                            PIC 9(05).
           03  IDX-DAY                         PIC 9(05).
           03  IDX-MED                         PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYYM                       PIC X(08).
           03  WRK-MEDKBN                      PIC X(01).
      *
           COPY    "CPMEDKBN.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  RYOCOM-REC.
           COPY    "CPRYOCOM.INC".
      *
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                        SECTION.
      *
           COPY    "CPORCSS008.INC".
      *
           COPY    "COMMON-SPA".
      *
       PROCEDURE                       DIVISION    USING
           SS008-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      SS008-OT
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    終了処理
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           MOVE    ZERO            TO  FLG-END
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
           PERFORM 900-NYUINACCT-KEY17-SEL-SEC
      *
           PERFORM 900-RYOCOM-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-RYOCOM  NOT =   ZERO )
                    OR   ( SS008-RC    NOT =   ZERO )
      *
               IF    ( RYOCOM-ZAISKBKBN    =   "2" )
                   PERFORM 2001-EDIT-MEDKBN-SEC
               END-IF
      *
               PERFORM 900-RYOCOM-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療区分編集処理
      *****************************************************************
       2001-EDIT-MEDKBN-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-MEDKBN
           PERFORM VARYING IDX-MED FROM    1   BY  1
                   UNTIL ( IDX-MED     >   MEDH20-MAX )
                    OR   ( WRK-MEDKBN  NOT =   SPACE )
               IF   ( RYOCOM-SRYYM >=  MEDH20-STYM   (IDX-MED))
                AND ( RYOCOM-SRYYM <=  MEDH20-EDYM   (IDX-MED))
                AND ( RYOCOM-ZAINUM
                                   =   MEDH20-ZAINUM (IDX-MED))
                   MOVE    MEDH20-MEDKBN (IDX-MED)
                                   TO  WRK-MEDKBN
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX-DAY FROM    1   BY  1
                   UNTIL ( IDX-DAY >   31 )
               IF    ( NACCT-DAY (IDX-DAY)     >   ZERO )
      *
                   IF    ( RYOCOM-MONTH        >   ZERO )
                    OR   ( RYOCOM-DAY(IDX-DAY) >   ZERO )
                       IF    ( SS008-MEDKBNDAY (IDX-DAY)
                                               =   SPACE )
                           MOVE    WRK-MEDKBN  TO  SS008-MEDKBNDAY
                                                          (IDX-DAY)
                       ELSE
                           IF    ( WRK-MEDKBN  >   SS008-MEDKBNDAY
                                                             (IDX-DAY))
                               MOVE    WRK-MEDKBN
                                               TO  SS008-MEDKBNDAY
                                                             (IDX-DAY)
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       2001-EDIT-MEDKBN-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    ZERO            TO  SS008-RC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    療養レセプトコメントマスタ検索処理
      *****************************************************************
       900-RYOCOM-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-RYOCOM
      *
           INITIALIZE                  RYOCOM-REC
           MOVE    SPA-HOSPNUM     TO  RYOCOM-HOSPNUM
           MOVE    "1"             TO  RYOCOM-NYUGAIKBN
           MOVE    SPA-PTID        TO  RYOCOM-PTID
           MOVE    SS008-SRYYM     TO  RYOCOM-SRYYM
           MOVE    RYOCOM-REC      TO  MCPDATA-REC
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  RYOCOM-REC
           ELSE
               MOVE    1           TO  FLG-RYOCOM
               INITIALIZE              RYOCOM-REC
           END-IF
      *
           .
       900-RYOCOM-KEY2-SEL-EXT.
            EXIT.
      *****************************************************************
      *    療養レセプトコメントマスタFETCH処理
      *****************************************************************
       900-RYOCOM-KEY2-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-RYOCOM
      *
           MOVE    "tbl_ryocom"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  RYOCOM-REC
           ELSE
               MOVE    1           TO  FLG-RYOCOM
               INITIALIZE              RYOCOM-REC
           END-IF
      *
           .
       900-RYOCOM-KEY2-FET-EXT.
            EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY17-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
      *
           INITIALIZE                          NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    SS008-SRYYM         TO  NACCT-SRYYM
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY17-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
