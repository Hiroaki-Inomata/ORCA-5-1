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
      *    診療報酬請求書     COPY (CPSKT4520TMP.INC)            *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SKT4520TMP-REC.
           03      SKT4520TMP-KEY.
             05    SKT4520TMP-SYUTURYOKU            PIC  9(01).
             05    SKT4520TMP-HKNJANUM              PIC  X(06).
             05    SKT4520TMP-ZAITAKU               PIC  X(01).
             05    SKT4520TMP-KYURATE               PIC  9(03).
           03    SKT4520TMP-PAGE-FLG1               PIC  9(01).
           03    SKT4520TMP-PAGE-FLG2               PIC  9(01).
           03    SKT4520TMP-SEIKYU-TBL              OCCURS 11.
               05    SKT4520TMP-NYUGAI-TBL          OCCURS 2.
                 07    SKT4520TMP-KENSU             PIC 9(05).
                 07    SKT4520TMP-NISSU             PIC 9(05).
                 07    SKT4520TMP-TENSU             PIC 9(09).
                 07    SKT4520TMP-ITBFTN            PIC 9(09).
               05    SKT4520TMP-SKENSU              PIC 9(05).
               05    SKT4520TMP-SNISSU              PIC 9(05).
               05    SKT4520TMP-SKINGK              PIC 9(09).
               05    SKT4520TMP-SFTNGK              PIC 9(09).
           03  SKT4520TMP-KOH-AREA.
               05  SKT4520TMP-KOH-TBL      OCCURS  20.
                   07  SKT4520TMP-KOH-KEY.
                       09  SKT4520TMP-KOHNUM        PIC  X(03).
                       09  SKT4520TMP-KOH-FTNRATE   PIC  9(01).
                   07  SKT4520TMP-K-NYUGAI-TBL  OCCURS  2.
                       09  SKT4520TMP-K-KENSU       PIC  9(05).
                       09  SKT4520TMP-K-NISSU       PIC  9(05).
                       09  SKT4520TMP-K-TENSU       PIC  9(09).
                       09  SKT4520TMP-K-ITBFTN      PIC  9(09).
                   07  SKT4520TMP-K-SKENSU          PIC  9(05).
                   07  SKT4520TMP-K-SNISSU          PIC  9(05).
                   07  SKT4520TMP-K-SKINGK          PIC  9(09).
                   07  SKT4520TMP-K-SFTNGK          PIC  9(09).
