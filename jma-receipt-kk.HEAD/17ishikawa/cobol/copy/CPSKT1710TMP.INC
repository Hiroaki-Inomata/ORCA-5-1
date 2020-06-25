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
      *  国保総括票         COPY (CPSKT1710TMP.INC)              *
      *                                                          *
      *  コピー句修正履歴                                        *
      * Maj/Min/Rev  修正者       日付      内容                 *
      ************************************************************
       01  SKT1710TMP-REC.
           03  SKT1710TMP-KEY.
               05  SKT1710TMP-SYUTURYOKU            PIC  9(01).
               05  SKT1710TMP-HKNJANUM              PIC  X(06).
               05  SKT1710TMP-RECEKA                PIC  X(02).
           03  SKT1700TMP-SEIKYU-TBL      OCCURS 13.
               05  SKT1700TMP-NYUGAIKBN   OCCURS 2.
                   07  SKT1710TMP-KENSU             PIC  9(05).
                   07  SKT1710TMP-NISSU             PIC  9(05).
                   07  SKT1710TMP-TENSU             PIC  9(09).
                   07  SKT1710TMP-YKZFTN            PIC  9(09).
                   07  SKT1710TMP-ITBFTN            PIC  9(09).
                   07  SKT1710TMP-TYOKI             PIC  9(09).
               05  SKT1710TMP-SKENSU                PIC  9(05).
               05  SKT1710TMP-SNISSU                PIC  9(05).
               05  SKT1710TMP-SKINGK                PIC  9(09).
               05  SKT1710TMP-SFTNGK                PIC  9(09).
           03  SKT1710TMP-ZAITAKU-TBL     OCCURS 6.
               05  SKT1710TMP-Z-KENSU               PIC  9(05).
               05  SKT1710TMP-Z-NISSU               PIC  9(05).
               05  SKT1710TMP-Z-TENSU               PIC  9(09).
               05  SKT1710TMP-Z-ITBFTN              PIC  9(09).
               05  SKT1710TMP-Z-TYOKI               PIC  9(09).
           03  SKT1710TMP-KOHI-TBL        OCCURS 9.
               05  SKT1710TMP-K-KENSU               PIC  9(05).
               05  SKT1710TMP-K-NISSU               PIC  9(05).
               05  SKT1710TMP-K-TENSU               PIC  9(09).
               05  SKT1710TMP-K-YKZFTN              PIC  9(09).
               05  SKT1710TMP-K-ITBFTN              PIC  9(09).
               05  SKT1710TMP-K-SKENSU              PIC  9(05).
               05  SKT1710TMP-K-SNISSU              PIC  9(05).
               05  SKT1710TMP-K-SKINGK              PIC  9(09).
               05  SKT1710TMP-K-SFTNGK              PIC  9(09).
