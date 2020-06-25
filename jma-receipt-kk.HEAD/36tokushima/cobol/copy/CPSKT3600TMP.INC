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
      *    診療報酬請求書     COPY (CPSKT3600TMP.INC)              *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SKT3600TMP-REC.
           03  SKT3600TMP-KEY.
               05  SKT3600TMP-PREFKBN               PIC  X(01).
               05  SKT3600TMP-ZAITAKU               PIC  X(01).
           03  SKT3600TMP-HKN-AREA  OCCURS 11.
               05  SKT3600TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT3600TMP-KENSU        PIC 9(05).
                   07  SKT3600TMP-NISSU        PIC 9(05).
                   07  SKT3600TMP-TENSU        PIC 9(09).
                   07  SKT3600TMP-ITBFTN       PIC 9(09).
               05  SKT3600TMP-SKENSU           PIC 9(05).
               05  SKT3600TMP-SNISSU           PIC 9(05).
               05  SKT3600TMP-SKINGK           PIC 9(09).
               05  SKT3600TMP-SFTNGK           PIC 9(09).
           03  SKT3600TMP-KOH-AREA.
               05  SKT3600TMP-KOH-TBL  OCCURS  20.
                   07  SKT3600TMP-KOHNUM           PIC X(03).
                   07  SKT3600TMP-KOHNAME          PIC X(16).
                   07  SKT3600TMP-K-NYUGAI-TBL  OCCURS  2.
                       09  SKT3600TMP-K-KENSU      PIC 9(05).
                       09  SKT3600TMP-K-NISSU      PIC 9(05).
                       09  SKT3600TMP-K-TENSU      PIC 9(09).
                       09  SKT3600TMP-K-ITBFTN     PIC 9(09).
                   07  SKT3600TMP-K-SKENSU         PIC 9(05).
                   07  SKT3600TMP-K-SNISSU         PIC 9(05).
                   07  SKT3600TMP-K-SKINGK         PIC 9(09).
                   07  SKT3600TMP-K-SFTNGK         PIC 9(09).
           03  SKT3600TMP-KESE-AREA  OCCURS  2.
               05  SKT3600TMP-KESE-NYUGAI  OCCURS  2.
                   07  SKT3600TMP-KESE-KENSU      PIC 9(05).
                   07  SKT3600TMP-KESE-NISSU      PIC 9(05).
                   07  SKT3600TMP-KESE-TENSU      PIC 9(09).
                   07  SKT3600TMP-KESE-ITBFTN     PIC 9(09).
               05  SKT3600TMP-KESE-SKENSU         PIC 9(05).
               05  SKT3600TMP-KESE-SNISSU         PIC 9(05).
               05  SKT3600TMP-KESE-SKINGK         PIC 9(09).
               05  SKT3600TMP-KESE-SFTNGK         PIC 9(09).
