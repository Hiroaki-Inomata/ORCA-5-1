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
      *    診療報酬請求書     COPY (CPSKT3750TMP.INC)              *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SKT3620TMP-REC.
           03  SKT3620TMP-KEY.
               05  SKT3620TMP-SYUTURYOKU            PIC  9(01).
               05  SKT3620TMP-HKNJANUM              PIC  X(06).
               05  SKT3620TMP-ZAITAKU               PIC  X(01).
               05  SKT3620TMP-KYURATE               PIC  9(03).
           03  SKT3620TMP-HKN-AREA  OCCURS  11.
               05  SKT3620TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT3620TMP-KENSU             PIC  9(05).
                   07  SKT3620TMP-NISSU             PIC  9(05).
                   07  SKT3620TMP-TENSU             PIC  9(09).
                   07  SKT3620TMP-ITBFTN            PIC  9(09).
               05  SKT3620TMP-SKENSU                PIC  9(05).
               05  SKT3620TMP-SNISSU                PIC  9(05).
               05  SKT3620TMP-SKINGK                PIC  9(09).
               05  SKT3620TMP-SFTNGK                PIC  9(09).
           03  SKT3620TMP-KOH-AREA.
               05  SKT3620TMP-KOH-TBL  OCCURS  20.
                   07  SKT3620TMP-KOHNUM                PIC  X(03).
                   07  SKT3620TMP-KOHNAME               PIC  X(16).
                   07  SKT3620TMP-KOH-NYUGAI-TBL OCCURS 2.
                       09  SKT3620TMP-K-KENSU           PIC  9(05).
                       09  SKT3620TMP-K-NISSU           PIC  9(05).
                       09  SKT3620TMP-K-TENSU           PIC  9(09).
                       09  SKT3620TMP-K-ITBFTN          PIC  9(09).
                   07  SKT3620TMP-K-SKENSU              PIC  9(05).
                   07  SKT3620TMP-K-SNISSU              PIC  9(05).
                   07  SKT3620TMP-K-SKINGK              PIC  9(09).
                   07  SKT3620TMP-K-SFTNGK              PIC  9(09).
