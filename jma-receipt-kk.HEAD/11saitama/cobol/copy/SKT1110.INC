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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    半田     03.03.07  新規
      ************************************************************
       01  SKT1110.
         03      SKT1110-NO           PIC X(04).
         03      SKT1110-SRYYM        PIC X(16).
         03      SKT1110-ADRS         PIC X(100).
         03      SKT1110-HOSPNAME     PIC X(100).
         03      SKT1110-KAISETUNAME  PIC X(80).
         03      SKT1110-TEL          PIC X(15).
         03      SKT1110-HKNJANAME    PIC X(26).
         03      SKT1110-HKNJANUM     PIC X(06).
         03      SKT1110-HOSPCDN      PIC X(07).
         03      SKT1110-SEIYMD       PIC X(22).
         03      SKT1110-ZAI1         PIC X(02).
         03      SKT1110-ZAI2         PIC X(02).
      *
         03      SKT1110-SEIKYU      OCCURS 10.
           05    SKT1110-NYUGAI-TBL  OCCURS 2.
             07  SKT1110-KENSU        PIC X(05).
             07  SKT1110-NISSU        PIC X(05).
             07  SKT1110-TENSU        PIC X(09).
             07  SKT1110-ITBFTN       PIC X(09).
           05    SKT1110-SKENSU       PIC X(05).
           05    SKT1110-SNISSU       PIC X(05).
           05    SKT1110-SKINGK       PIC X(09).
           05    SKT1110-SFTNGK       PIC X(09).
      *
         03      SKT1110-KYURATE-TBL  OCCURS 2.
           05    SKT1110-KYURATE      PIC X(02).
