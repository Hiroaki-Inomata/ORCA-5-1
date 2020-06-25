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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSONPRTSET.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ONLINE帳票再印刷準備処理
      *  コンポーネント名  : GROUP & UUID 作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/03/10    NACL−竹田　  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *    ＵＩＤ取得用エリア
       01 UIDIO-AREA.
           03  C-RET   PIC X(2).
           03  C-UID   PIC X(36).
       01  ST          PIC 9(2).
      *
      *
       COPY    "CPORCSMCNDATE.INC".
       COPY    "MCPAREA".
       COPY    "MONBLOB".
      *
      **************************************************************************
      *
       LINKAGE                 SECTION.
           COPY    "COMMON-SPA".
      *
      **************************************************************************
       PROCEDURE           DIVISION
               USING
               SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           DISPLAY "ORCSONPRTSET ST ="
      *
           MOVE    SPACE           TO  SPA-PUTTYPE
           MOVE    SPACE           TO  SPA-PRT-UUID
      *
           CALL    "ORCSMCNDATE"       USING ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD    TO  SPA-PRT-GROUP (1:8)
           MOVE    SMCNDATE-HMS    TO  SPA-PRT-GROUP (9:)
      *
      * UUID GET
           IF    ( SPA-PRTCONF    =    "2" OR "3" )
      *
               MOVE    "MONBLOBNEW"    TO  MCP-FUNC
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    "monblob"       TO  MCP-TABLE
               CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MONBLOB
               MOVE    MONBLOB-ID      TO  SPA-PRT-UUID
      D        DISPLAY "MONBLOB-ID " SPA-PRT-UUID
           ELSE
               INITIALIZE     UIDIO-AREA
               CALL    "orcuidset" USING ST UIDIO-AREA
      D        DISPLAY "UUID CALL" UIDIO-AREA
               MOVE    C-UID       TO  SPA-PRT-UUID
           END-IF
      *
           MOVE    SPACE           TO  SPA-PRT-FLG
           MOVE    ZERO            TO  SPA-CLIENT-TEMP-FLG
      *
           DISPLAY "ORCSONPRTSET ED ="
      *???     
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
