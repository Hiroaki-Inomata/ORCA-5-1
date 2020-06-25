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
      *    診療報酬請求書     COPY (CPSKT1510TMP.INC)              *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SKT1510TMP-REC.
           03  SKT1510TMP-KEY.
               05  SKT1510TMP-SYUTURYOKU            PIC  9(01).
               05  SKT1510TMP-HKNJANUM              PIC  X(06).
               05  SKT1510TMP-ZAITAKU               PIC  X(01).
               05  SKT1510TMP-GENMEN                PIC  X(05).
           03  SKT1510TMP-HKN-AREA  OCCURS  11.
               05  SKT1510TMP-NYUGAI-TBL OCCURS 2.
                   07  SKT1510TMP-KENSU             PIC  9(05).
                   07  SKT1510TMP-NISSU             PIC  9(05).
                   07  SKT1510TMP-TENSU             PIC  9(09).
                   07  SKT1510TMP-ITBFTN            PIC  9(09).
               05  SKT1510TMP-SKENSU                PIC  9(05).
               05  SKT1510TMP-SNISSU                PIC  9(05).
               05  SKT1510TMP-SKINGK                PIC  9(09).
               05  SKT1510TMP-SFTNGK                PIC  9(09).
           03  SKT1510TMP-KOH-AREA.
               05  SKT1510TMP-KOH-TBL  OCCURS  20.
                   07  SKT1510TMP-KOHNUM            PIC  X(03).
                   07  SKT1510TMP-KOH-NYUGAI-TBL OCCURS 2.
                       09  SKT1510TMP-K-KENSU       PIC  9(05).
                       09  SKT1510TMP-K-NISSU       PIC  9(05).
                       09  SKT1510TMP-K-TENSU       PIC  9(09).
                       09  SKT1510TMP-K-ITBFTN      PIC  9(09).
                   07  SKT1510TMP-K-SKENSU          PIC  9(05).
                   07  SKT1510TMP-K-SNISSU          PIC  9(05).
                   07  SKT1510TMP-K-SKINGK          PIC  9(09).
                   07  SKT1510TMP-K-SFTNGK          PIC  9(09).
           03  SKT1510TMP-FUKUSI-AREA OCCURS 6.
               05  SKT1510TMP-F-KENSU               PIC  9(05).
               05  SKT1510TMP-F-TENSU               PIC  9(09).
               05  SKT1510TMP-F-SKENSU              PIC  9(05).
               05  SKT1510TMP-F-SKINGK              PIC  9(09).
               05  SKT1510TMP-F-SFTNGK              PIC  9(09).
           03  SKT1510TMP-KOUGK-NYUGAI-TBL  OCCURS 2.
               05  SKT1510TMP-KOUGK-KENSU           PIC  9(05).
