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
      *    診療報酬請求書     COPY (CPSKT0110TMP.INC)            *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SKT0110TMP-REC.
           03  SKT0110TMP-KEY.
               05  SKT0110TMP-SYUTURYOKU            PIC  9(01).
               05  SKT0110TMP-HKNJANUM              PIC  9(06).
               05  SKT0110TMP-ZAITAKU               PIC  X(01).
           03  SKT0110TMP-PAGE-FLG                  PIC  9(01).
           03  SKT0110TMP-TBL	OCCURS  12.
               05  SKT0110TMP-NYUGAI-TBL	OCCURS  2.
                  07  SKT0110TMP-KENSU              PIC  9(05).
                  07  SKT0110TMP-NISSU              PIC  9(05).
                  07  SKT0110TMP-TENSU              PIC  9(09).
                  07  SKT0110TMP-ITBFTN             PIC  9(09).
               05  SKT0110TMP-SKENSU                PIC  9(05).
               05  SKT0110TMP-SNISSU                PIC  9(05).
               05  SKT0110TMP-SKINGK                PIC  9(09).
               05  SKT0110TMP-SFTNGK                PIC  9(09).
           03  SKT0110TMP-KYURATE-TBL  OCCURS  2.
               05  SKT0110TMP-KYURATE               PIC  9(02).
           03  SKT0110TMP-DORO-TBL.
               05  SKT0110TMP-D-KENSU               PIC  9(05).
               05  SKT0110TMP-D-NISSU               PIC  9(05).
               05  SKT0110TMP-D-TENSU               PIC  9(09).
               05  SKT0110TMP-D-ITBFTN              PIC  9(10).
               05  SKT0110TMP-D-SKENSU              PIC  9(05).
               05  SKT0110TMP-D-SNISSU              PIC  9(05).
               05  SKT0110TMP-D-SKINGK              PIC  9(09).
               05  SKT0110TMP-D-SFTNGK              PIC  9(09).
           03  SKT0110TMP-TYOKI-TBL  OCCURS  2.
               05  SKT0110TMP-T-RATE                PIC  9(02).
               05  SKT0110TMP-T-KENSU               PIC  9(05).
               05  SKT0110TMP-T-NISSU               PIC  9(05).
               05  SKT0110TMP-T-TENSU               PIC  9(09).
               05  SKT0110TMP-T-ITBFTN              PIC  9(09).
           03  SKT0110TMP-KOH-AREA.
               05  SKT0110TMP-KOH-TBL     OCCURS  40.
                   07  SKT0110TMP-KOH-KEY.
                       09  SKT0110TMP-KOHNUM        PIC  X(03).
                       09  SKT0110TMP-RJN-19-KBN    PIC  X(03).
                       09  SKT0110TMP-RENNUM        PIC  9(02).
                   07  SKT0110TMP-K-RATE-TBL        OCCURS  2.
                       09  SKT0110TMP-K-NYUGAI-TBL  OCCURS  2.
                           11  SKT0110TMP-K-RATE    PIC  9(03).
                           11  SKT0110TMP-K-KENSU   PIC  9(05).
                           11  SKT0110TMP-K-NISSU   PIC  9(05).
                           11  SKT0110TMP-K-TENSU   PIC  9(09).
                           11  SKT0110TMP-K-ITBFTN  PIC  9(09).
                       09  SKT0110TMP-K-SKENSU      PIC  9(05).
                       09  SKT0110TMP-K-SNISSU      PIC  9(05).
                       09  SKT0110TMP-K-SKINGK      PIC  9(09).
                       09  SKT0110TMP-K-SFTNGK      PIC  9(09).
