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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGN21ID2.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 薬剤情報設定
      *  コンポーネント名  : 確認画面２（Ｎ２１ＩＤ２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/05/11    NACL−森脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                                      DIVISION.
      *FILE                                      SECTION.
      *
      *
       WORKING-STORAGE                           SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "N21COMMON-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                           PIC 9(01).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "N21.INC".
           COPY    "N22.INC".
           COPY    "N23.INC".
           COPY    "N29.INC".
           COPY    "N21ERR.INC".
           COPY    "N21ID1.INC".
           COPY    "N21ID2.INC".
           COPY    "N21FIL.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-N21KYOUTU
      * 
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
           END-EVALUATE.
      *
           MOVE    SPA-N21KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                              SECTION.
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                               SECTION.
      *
           EVALUATE    MCP-EVENT                 ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"                 ALSO    "B01"
                   MOVE    SPACE                 TO  SPA-N21ID1-FLG
                   PERFORM 210-BACK
      *     プレビュー
               WHEN    "CLICKED"                 ALSO    "B10"
                   MOVE    "PV"                  TO  SPA-N21ID1-FLG
                   PERFORM 210-BACK
      *     印刷しない
               WHEN    "CLICKED"                 ALSO    "B11"
                   MOVE    "NO"                  TO  SPA-N21ID1-FLG
                   PERFORM 210-BACK
      *     印刷する
               WHEN    "CLICKED"                 ALSO    "B12"
                   MOVE    "OK"                  TO  SPA-N21ID1-FLG
                   PERFORM 210-BACK
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                                  SECTION.
      *
           MOVE    SPA-MOTOPG          TO  SPA-SAKIPG
           MOVE    "N21ID2"            TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                             TO  FLG-END
           .
      *
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                            SECTION.
      *
           MOVE   "PUTWINDOW"                    TO  MCP-FUNC.
           CALL   "ORCDBMAIN"                    USING
                                                     MCPAREA
                                                     MCPDATA-REC
                                                     SPA-AREA
      *
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
      *
