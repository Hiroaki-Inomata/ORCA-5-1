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
      *    総括一時ファイル     COPY (CPSKT2600TMP.INC)
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         10/11/02  特別療養費国保対応
      *  01.00.02    吉川         10/11/24  open-cobol1.0対応(多重定義修正)
      *  01.00.03    吉川         11/04/22  東北地方太平洋沖地震対応
      *  01.00.04    門間         18/12/04  修正履歴コメント削除
      ********************************************************************
       01  SKT2600TMP-REC.
           03  SKT2600TMP-KEY.
               05  SKT2600TMP-NYUGAIKBN        PIC  9(01).
           03  SKT2600TMP-DATA   OCCURS  8.
               05  SKT2600TMP-NYUGAI-TBL   OCCURS  2.
                   07  SKT2600TMP-KENSU        PIC  9(06).
                   07  SKT2600TMP-NISSU        PIC  9(06).
                   07  SKT2600TMP-TENSU        PIC  9(10).
                   07  SKT2600TMP-ITBFTN       PIC  9(10).
                   07  SKT2600TMP-TAKOH        PIC  9(02).
                   07  SKT2600TMP-TYOKI        PIC  9(02).
                   07  SKT2600TMP-GENBAKU      PIC  9(02).
                   07  SKT2600TMP-S-KENSU      PIC  9(06).
                   07  SKT2600TMP-S-NISSU      PIC  9(06).
                   07  SKT2600TMP-S-KINGAKU    PIC  9(10).
                   07  SKT2600TMP-S-FTNGAKU    PIC  9(10).
           03  SKT2600TMP-GENMEN-TBL.
               05  SKT2600TMP-G-KENSU          PIC  9(03).
               05  SKT2600TMP-G-TENSU          PIC  9(07).
           03  SKT2600TMP-T-KENSU              PIC  9(03).
           03  SKT2600TMP-T-TENSU              PIC  9(10).
           03  SKT2600TMP-SAIGAI               PIC  X(01).
           03  SKT2600TMP-SAIGAI-TBL       OCCURS  2.
               05  SKT2600TMP-SAI-KENSU        PIC  9(06).
               05  SKT2600TMP-SAI-TENSU        PIC  9(10).
