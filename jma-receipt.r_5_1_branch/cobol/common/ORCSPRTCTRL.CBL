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
       PROGRAM-ID.             ORCSPRTCTRL.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : クライアント印刷制御情報組み立て
      *  コンポーネント名  : GROUP & UUID 作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/03/10    NACL−竹田　  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-太田    14/08/06  4.8クライアント印刷対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       DATA                    DIVISION.
       FILE                        SECTION.
       WORKING-STORAGE         SECTION.
      **************************************************************************
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                 SECTION.
           COPY    "COMMON-SPA".
           COPY    "CPORCSPRTCTRL.INC".
           COPY    "MCPAREA".
      *
      **************************************************************************
       PROCEDURE           DIVISION
               USING
               SPA-AREA
               ORCSPRTCTRLAREA
               MCPAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
         IF  ( ORCSPRTCTRL-BATCH-FLG NOT   =   1  )
               MOVE    1               TO  SPA-CLIENT-PRT-FLG
         END-IF
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
