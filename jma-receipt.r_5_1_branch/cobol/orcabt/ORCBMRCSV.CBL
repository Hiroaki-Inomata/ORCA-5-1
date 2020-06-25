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
       PROGRAM-ID.             ORCBMRCSV.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : 診療情報データ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/07/25    NACL-伊藤     改修
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *****************************************************************
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  PARA-AREA.
           03  PARA-VERSION-KBN    PIC X(01).
           03  PARA-ORCA-VERSION   PIC X(10).
           03  PARA-DOUJITU        PIC X(01).
           03  PARA-TEMP           PIC X(128).
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *     COPY    "MCPDATA.INC".
      *     COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      *
       PROCEDURE                   DIVISION
                                   USING
                                       COMMAND-PARAM.
      *
      ****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE  PARA-AREA
           INITIALIZE  SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    SPA-HOSPNUM
                                               SPA-PTID
                                               SPA-NYUGAIKBN
                                               SPA-SRYYMD
                                               PARA-VERSION-KBN
                                               SPA-DRCD
                                               SPA-OPID
                                               PARA-DOUJITU
                                               PARA-ORCA-VERSION
                                               PARA-TEMP
           END-UNSTRING
      *
           IF      PARA-VERSION-KBN    =   "1"
               CALL    "ORCBMRCSV09"   USING   COMMAND-PARAM
           ELSE
               CALL    "ORCBMRCSV10L"  USING   COMMAND-PARAM
           END-IF
      *
           STOP    RUN
           .
