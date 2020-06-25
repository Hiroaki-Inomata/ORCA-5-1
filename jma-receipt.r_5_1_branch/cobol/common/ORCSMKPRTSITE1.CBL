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
       PROGRAM-ID.             ORCSMKPRTSITE1.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : オンライン帳票印刷サブ
      *                    : (再印刷用DB書き込み追加)
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/09/12    NACL-竹田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付    内容
      *  04.07.00    NACL-竹田  11/03/20  クライアント印刷対応
      *  04.07.00    NACL-竹田  13/04/26  複数枚対応
      *  05.00.00    NACL-太田  17/06/30  MKPRT1に処理を移動
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       DATA                        DIVISION.
       FILE                        SECTION.
       WORKING-STORAGE             SECTION.
        LINKAGE                    SECTION.
           COPY  "CPORCSMKPRT.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSMKPRTAREA
           SPA-AREA.
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    "__SITE__"      TO  MKPRT-DEF
           CALL    "ORCSMKPRT1"        USING
                                       ORCSMKPRTAREA
                                       SPA-AREA.
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
