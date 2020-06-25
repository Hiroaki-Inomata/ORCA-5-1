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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGEID1.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＣＳＶ作成
      *  コンポーネント名  : 確認画面１（ＥＩＤ１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/03/12    ひさなが　    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *      
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
       01  SPA-E02.
           03  SPA-E02-AREA.
               05  SPA-GMN-PAGE               PIC 9(02).
               05  SPA-GMN-MAX-A              PIC 9(03).
               05  SPA-GMN-MAX-B              PIC 9(03).
               05  SPA-GMN-CUR                PIC 9(03).
               05  SPA-GMN-KBN                PIC X(02).
               05  SPA-GMN-RUN                PIC 9(01).
               05  SPA-GMN-AREA.
                   07  SPA-GMN-SRYYM          PIC X(06).
                   07  SPA-GMN-TBLNM          PIC X(20).
                   07  SPA-GMN-FILE-ID        PIC X(128).
                   07  SPA-GMN-FILE-KEY       PIC X(64).
               05  SPA-NAI-AREA.
                   07  SPA-NAI-MOTOPG         PIC X(08).
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
        LINKAGE                                  SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "E01.INC".
           COPY    "E02.INC".
           COPY    "EERR.INC".
           COPY    "EID1.INC".
           COPY    "EID2.INC".
           COPY    "F01.INC".
           COPY    "FERR.INC".
           COPY    "FID1.INC".
           COPY    "FID2.INC".
      *
       PROCEDURE                                 DIVISION    USING
                                                             MCPAREA
                                                             SPAAREA
                                                             LINKAREA
                                                             SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                              SECTION.
      *
           MOVE    SPA-COMMON                    TO  SPA-AREA.
           MOVE    SPA-FREE                      TO  SPA-E02.
      * 
           EVALUATE    MCP-STATUS                ALSO    MCP-EVENT
               WHEN    "LINK"                    ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"                   ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
           END-EVALUATE.
      *
           MOVE    SPA-AREA                      TO  SPA-COMMON.
           MOVE    SPA-E02                       TO  SPA-FREE.
      *
           EXIT    PROGRAM.
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                              SECTION.
      *
      *
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
                   MOVE    SPACE                 TO  SPA-GMN-KBN
                   PERFORM 210-BACK
      *     登録
               WHEN    "CLICKED"                 ALSO    "B12"
                   MOVE    "OK"                  TO  SPA-GMN-KBN
                   PERFORM 210-BACK
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                                  SECTION.
      *
           MOVE    SPA-MOTOPG          TO  SPA-SAKIPG.
           MOVE    "EID1"              TO  SPA-MOTOPG.
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
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
                                                     SPA-AREA.
      *
       900-PUT-WINDOW-EXT.
           EXIT.
      *
