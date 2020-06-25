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
      ************************************************************
      *    総括一時ファイル     COPY (CPSKT2630TMP.INC)
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     門間         08/09/25  公費名称追加
      * 01.00.02     吉川         11/04/22  東北地方太平洋沖地震対応
      * 01.00.03     門間         18/12/04  修正履歴コメント削除
      ************************************************************
       01  SKT2630TMP-REC.
           03  SKT2630TMP-KEY.
               05  SKT2630TMP-SYUTURYOKU       PIC  9(01).
               05  SKT2630TMP-HKNJANUM         PIC  X(08).
               05  SKT2630TMP-ZAITAKU          PIC  9(01).
           03  SKT2630TMP-DATA   OCCURS  14.
               05  SKT2630TMP-NYUGAI-TBL   OCCURS  2.
                   07  SKT2630TMP-KENSU        PIC  9(06).
                   07  SKT2630TMP-NISSU        PIC  9(06).
                   07  SKT2630TMP-TENSU        PIC  9(10).
                   07  SKT2630TMP-ITBFTN       PIC  9(10).
               05  SKT2630TMP-S-KENSU          PIC  9(06).
               05  SKT2630TMP-S-NISSU          PIC  9(06).
               05  SKT2630TMP-S-KINGAKU        PIC  9(10).
               05  SKT2630TMP-S-FTNGAKU        PIC  9(10).
           03  SKT2630TMP-KOH   OCCURS  20.
               05  SKT2630TMP-KOHNUM           PIC  X(03).
               05  SKT2630TMP-KOHNAME          PIC  X(10).
               05  SKT2630TMP-K-NYUGAI-TBL   OCCURS  2.
                   07  SKT2630TMP-KKENSU       PIC  9(06).
                   07  SKT2630TMP-KNISSU       PIC  9(06).
                   07  SKT2630TMP-KTENSU       PIC  9(10).
                   07  SKT2630TMP-KITBFTN      PIC  9(10).
               05  SKT2630TMP-S-KKENSU         PIC  9(06).
               05  SKT2630TMP-S-KNISSU         PIC  9(06).
               05  SKT2630TMP-S-KKINGAKU       PIC  9(10).
               05  SKT2630TMP-S-KFTNGAKU       PIC  9(10).
           03  SKT2630TMP-SAIGAI               PIC  X(01).
           03  SKT2630TMP-SAIGAI-TBL       OCCURS  2.
               05  SKT2630TMP-SAI-NYUGAI-TBL  OCCURS  2.
                   07  SKT2630TMP-SAI-KENSU    PIC  9(06).
                   07  SKT2630TMP-SAI-TENSU    PIC  9(10).
