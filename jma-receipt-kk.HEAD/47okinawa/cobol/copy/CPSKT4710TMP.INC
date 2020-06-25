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
      *    診療報酬請求書     COPY (CPSKT4710TMP.INC)              *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SKT4710TMP-REC.
           03  SKT4710TMP-KEY.
               05  SKT4710TMP-SYUTURYOKU            PIC  9(02).
               05  SKT4710TMP-HKNJANUM              PIC  X(06).
               05  SKT4710TMP-ZAITAKU               PIC  X(01).
               05  SKT4710TMP-KYURATE               PIC  9(02).
               05  SKT4710TMP-RECEKA                PIC  X(02).
           03  SKT4710TMP-PAGE-FLG2                 PIC  9(01).
           03  SKT4710TMP-HKN-AREA  OCCURS  12.
               05  SKT4710TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT4710TMP-KENSU             PIC  9(05).
                   07  SKT4710TMP-NISSU             PIC  9(05).
                   07  SKT4710TMP-TENSU             PIC  9(09).
                   07  SKT4710TMP-ITBFTN            PIC  9(09).
               05  SKT4710TMP-SKENSU                PIC  9(05).
               05  SKT4710TMP-SNISSU                PIC  9(05).
               05  SKT4710TMP-SKINGK                PIC  9(09).
               05  SKT4710TMP-SFTNGK                PIC  9(09).
           03  SKT4710TMP-KOH-AREA.
               05  SKT4710TMP-KOH-TBL  OCCURS  20.
                   07  SKT4710TMP-KOHNUM              PIC  X(03).
                   07  SKT4710TMP-HKNBETU             PIC  9(01).
                   07  SKT4710TMP-KOH-NYUGAI-TBL OCCURS 2.
                       09  SKT4710TMP-K-KENSU         PIC  9(05).
                       09  SKT4710TMP-K-NISSU         PIC  9(05).
                       09  SKT4710TMP-K-TENSU         PIC  9(09).
                       09  SKT4710TMP-K-ITBFTN        PIC  9(09).
                   07  SKT4710TMP-K-SKENSU            PIC  9(05).
                   07  SKT4710TMP-K-SNISSU            PIC  9(05).
                   07  SKT4710TMP-K-SKINGK            PIC  9(09).
                   07  SKT4710TMP-K-SFTNGK            PIC  9(09).
