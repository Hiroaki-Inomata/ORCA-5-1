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
       PROGRAM-ID.             ORCSS006.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 入院請求期間編集
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
           03  FLG-PTNYUINRRK                  PIC 9(01).
           03  FLG-SKYST-TAIIN                 PIC 9(01).
           03  FLG-SKYST-NYUIN                 PIC 9(01).
           03  FLG-SKYED-TAIIN                 PIC 9(01).
           03  FLG-SKYED-NYUIN                 PIC 9(01).
           03  FLG-1DAY                        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                            PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SKYST-SKJ                   PIC X(01).
           03  WRK-SKYED-SKJ                   PIC X(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
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
           COPY    "CPORCSS006.INC".
      *
           COPY    "COMMON-SPA".
      *
       PROCEDURE                       DIVISION    USING
           SS006-AREA
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
           INITIALIZE                      SS006-OT
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           PERFORM 200-MAIN-SEC
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
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 900-PTNYUINRRK-KEY24-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
      *
               MOVE    ZERO            TO  FLG-1DAY
      *
               IF    ( SS006-SKYSTYMD  =   SS006-SKYEDYMD )
                   MOVE    1           TO  FLG-1DAY
               END-IF
      *
               IF    ( FLG-1DAY            =   1 )
                AND  ( SS006-SKYKBN        =   "1" )
                   CONTINUE
               ELSE
                   IF    ( SS006-SKYSTYMD  =   PTNYUINRRK-TAIINYMD )
                       MOVE    1           TO  FLG-SKYST-TAIIN
                   END-IF
               END-IF
      *
               IF    ( SS006-SKYSTYMD  =   PTNYUINRRK-NYUINYMD )
                   MOVE    1           TO  FLG-SKYST-NYUIN
                   EVALUATE    PTNYUINRRK-TIMEKBN-IN
                   WHEN    "1"
                   WHEN    "2"
                   WHEN    "3"
                       MOVE    PTNYUINRRK-TIMEKBN-IN
                                       TO  WRK-SKYST-SKJ
                   WHEN    OTHER
                       MOVE    "0"     TO  WRK-SKYST-SKJ
                   END-EVALUATE
               END-IF
      *
               IF    ( SS006-SKYKBN        =   "2" )
                   CONTINUE
               ELSE
                   IF    ( SS006-SKYEDYMD  =   PTNYUINRRK-TAIINYMD )
                       MOVE    1           TO  FLG-SKYED-TAIIN
                   END-IF
      *
                   IF    ( SS006-SKYEDYMD  =   PTNYUINRRK-NYUINYMD )
                    AND  ( PTNYUINRRK-TAIINYMD >   SS006-SKYEDYMD )
                       MOVE    1           TO  FLG-SKYED-NYUIN
                       EVALUATE    PTNYUINRRK-TIMEKBN-IN
                       WHEN    "1"
                           MOVE    "0"     TO  WRK-SKYED-SKJ
                       WHEN    "2"
                           MOVE    "1"     TO  WRK-SKYED-SKJ
                       WHEN    "3"
                           MOVE    "2"     TO  WRK-SKYED-SKJ
                       WHEN    OTHER
                           MOVE    "3"     TO  WRK-SKYED-SKJ
                       END-EVALUATE
                   END-IF
               END-IF
      *
               PERFORM 900-PTNYUINRRK-KEY24-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
           MOVE    "key24"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-SKYST-TAIIN     =   1 )
            AND  ( FLG-SKYST-NYUIN     =   1 )
               MOVE    "1"             TO  SS006-DOJITUFLG-ST
               MOVE    WRK-SKYST-SKJ   TO  SS006-TIMEKBN-ST
           END-IF
      *
           IF    ( FLG-SKYED-TAIIN     =   1 )
            AND  ( FLG-SKYED-NYUIN     =   1 )
               MOVE    "1"             TO  SS006-DOJITUFLG-ED
               MOVE    WRK-SKYED-SKJ   TO  SS006-TIMEKBN-ED
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *     PERFORM 800-DISPLAY-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       800-DISPLAY-SEC                     SECTION.
      *
      **     DISPLAY "FLG-SKYST-TAIIN =" FLG-SKYST-TAIIN
      **     DISPLAY "FLG-SKYST-NYUIN =" FLG-SKYST-NYUIN
      **     DISPLAY "FLG-SKYED-TAIIN =" FLG-SKYED-TAIIN
      **     DISPLAY "FLG-SKYED-NYUIN =" FLG-SKYED-NYUIN
      **     DISPLAY "WRK-SKYST-SKJ   =" WRK-SKYST-SKJ
      **     DISPLAY "WRK-SKYED-SKJ   =" WRK-SKYED-SKJ
      *
      **     DISPLAY "======================"
      **     DISPLAY "SS006-SKYSTYMD       =" SS006-SKYSTYMD
      **     DISPLAY "SS006-DOJITUFLG-ST   =" SS006-DOJITUFLG-ST
      **     DISPLAY "SS006-TIMEKBN-ST     =" SS006-TIMEKBN-ST
      **     DISPLAY "SS006-SKYEDYMD       =" SS006-SKYEDYMD
      **     DISPLAY "SS006-DOJITUFLG-ED   =" SS006-DOJITUFLG-ED
      **     DISPLAY "SS006-TIMEKBN-ED     =" SS006-TIMEKBN-ED
      **     DISPLAY "======================"
      *
           .
       800-DISPLAY-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY24-SEL-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNYUINRRK
      *
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM     TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID        TO  PTNYUINRRK-PTID
           MOVE    SS006-SKYEDYMD  TO  PTNYUINRRK-NYUINYMD
           MOVE    SS006-SKYSTYMD  TO  PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
           MOVE    "key24"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               MOVE    1           TO  FLG-PTNYUINRRK
               INITIALIZE              PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY24-SEL-EXT.
            EXIT.
      *****************************************************************
      *    入院履歴FETCH理
      *****************************************************************
       900-PTNYUINRRK-KEY24-FET-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNYUINRRK
      *
           MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
           MOVE    "key24"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               MOVE    1           TO  FLG-PTNYUINRRK
               INITIALIZE              PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY24-FET-EXT.
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
