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
       PROGRAM-ID.             ORCHCDUMMY.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム共通
      *  コンポーネント名  : 印刷ダミー処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  11/03/30    NACL-竹田 新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-太田    14/08/05  4.8クライアント印刷対応
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
      D    DISPLAY     "SPA-PUTTYPE    ["  SPA-PUTTYPE "]"
      *
           EVALUATE    TRUE
           WHEN  ( SPA-API-FLG         =   ZERO )
           WHEN  ( SPA-PRTCONF         =   "2" OR "3" )
      *
               MOVE    3               TO  SPA-CLIENT-PRT-FLG
      *
               INITIALIZE                  ORCSMKPRTAREA
               MOVE    "HC01.sh"       TO  MKPRT-ID
               MOVE    "ORCDUMMMY.red" TO  MKPRT-DIA
               MOVE    SPACE           TO  MKPRT-DEF
               MOVE    "DUMMY"         TO  MKPRT-DAT
               MOVE    "DUMMY"         TO  MKPRT-PRTNM
               CALL    "ORCSMKPRT1"        USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
           WHEN  ( SPA-API-FLG         =   1 )
      *
               MOVE    SPACE           TO  SHELLTBL
               INITIALIZE                  SHELLTBL
      *
               MOVE    "rpmain1.sh"    TO  SHELLTBL-NAME
               MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG1
               MOVE    SPA-TERMID      TO  SHELLTBL-ARG2
               MOVE    SPA-OPID        TO  SHELLTBL-ARG3
               MOVE    SPA-SYSYMD      TO  SHELLTBL-ARG4
               MOVE    SPA-PRT-UUID    TO  SHELLTBL-ARG5
      *
               MOVE    SHELLTBL        TO  MCPDATA-REC
               MOVE    "SHELL"         TO  MCP-FUNC
               MOVE    "shell"         TO  MCP-TABLE
               MOVE    "daily"         TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           WHEN    OTHER
               CALL    "ORCSAPIFMTERM"     USING
                                           SPA-AREA
           END-EVALUATE
      *
           IF    ( SPA-PRTCONF     =   "1" )
               MOVE    ZERO            TO  SPA-CLIENT-PRT-FLG
           ELSE
               MOVE    2               TO  SPA-CLIENT-PRT-FLG
           END-IF
           MOVE    ZERO                TO  SPA-CLIENT-TEMP-FLG
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *
