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
       PROGRAM-ID.             ORCHCN03S03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : 請求書兼領収書請求期間編集
      *  管理者            : 
      *  作成日付    作業者        記述
      *  2013/06/03  NACL−太田   新規作成
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
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDXD                        PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-SRYYMD.
               05  WRK-SRYYM               PIC X(06).
               05  WRK-SRYDD               PIC 9(02).
      *
       01  CONST-AREA.
           03  CONST-SYU-MAX               PIC 9(03)   VALUE 10.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
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
           COPY    "CPORCHCN03S03.INC".
      *
       PROCEDURE                       DIVISION    USING
                                       SPA-AREA
                                       ORCHCN03S03AREA.
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
      *                                WRK-AREA
                                       ORCHCN03S03-OT
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
           MOVE    "99999999"          TO  ORCHCN03S03-SKYSTYMD
           MOVE    "00000000"          TO  ORCHCN03S03-SKYEDYMD
      *
      *    請求期間編集処理
           PERFORM 2001-SEIKYU-KIKAN-EDIT-SEC
      *
           IF    ( ORCHCN03S03-SYUSTYMD    >   ORCHCN03S03-SKYSTYMD )
               MOVE    ORCHCN03S03-SYUSTYMD
                                           TO  ORCHCN03S03-SKYSTYMD
           END-IF
      *
           IF    ( ORCHCN03S03-SYUEDYMD    <   ORCHCN03S03-SKYEDYMD )
               MOVE    ORCHCN03S03-SYUEDYMD
                                           TO  ORCHCN03S03-SKYEDYMD
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求期間編集処理
      *****************************************************************
       2001-SEIKYU-KIKAN-EDIT-SEC      SECTION.
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   CONST-SYU-MAX )
      *
               IF    ( ORCHCN03S03-DENPNUM (IDXD)  NOT =   ZERO )
                   PERFORM 20011-SEIKYU-KIKAN-EDIT-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       2001-SEIKYU-KIKAN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    診療科・保険組合せ別集計処理
      *****************************************************************
       20011-SEIKYU-KIKAN-EDIT-SEC     SECTION.
      *
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
      *    PERFORM 900-HKNCOMBI-KEY-SEL-SEC
      *    IF    ( FLG-HKNCOMBI    =   ZERO )
      *        IF    ( COMB-TEKSTYMD   <   ORCHCN03S03-SKYSTYMD )
      *            MOVE    COMB-TEKSTYMD   TO  ORCHCN03S03-SKYSTYMD
      *        END-IF
      *
      *        IF    ( COMB-TEKEDYMD   >   ORCHCN03S03-SKYEDYMD )
      *            MOVE    COMB-TEKEDYMD   TO  ORCHCN03S03-SKYEDYMD
      *        END-IF
      *    END-IF
      *
           MOVE    SYU-SRYYMD          TO  WRK-SRYYMD
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   31 )
      *
             IF  ( SYU-DAY (IDX0)  >   ZERO )
      *
                 MOVE    IDX0              TO  WRK-SRYDD
      *
                 IF    ( WRK-SRYYMD    <   ORCHCN03S03-SKYSTYMD )
                     MOVE    WRK-SRYYMD    TO  ORCHCN03S03-SKYSTYMD
                 END-IF
      *
                 IF    ( WRK-SRYYMD    >   ORCHCN03S03-SKYEDYMD )
                     MOVE    WRK-SRYYMD    TO  ORCHCN03S03-SKYEDYMD
                 END-IF
      *
             END-IF
      *
           END-PERFORM
      *
           .
       20011-SEIKYU-KIKAN-EDIT-EXT.
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
      *    収納テーブル検索処理
      *****************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    ORCHCN03S03-PTID    TO  SYU-PTID
           MOVE    ORCHCN03S03-DENPNUM (IDXD)
                                       TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ検索処理(KEY)
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SYU-PTID            TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
            ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
            END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
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
       910-DB-SELECT-EXT.
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
           .
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
           .
