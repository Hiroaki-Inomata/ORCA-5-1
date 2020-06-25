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
      *    診療報酬請求書     COPY (CPSKT2915TMP.INC)              *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SKT2915TMP-REC.
           03  SKT2915TMP-KEY.
               05  SKT2915TMP-SYUTURYOKU            PIC  9(01).
               05  SKT2915TMP-HKNJANUM              PIC  X(08).
               05  SKT2915TMP-ZAITAKU               PIC  X(01).
               05  SKT2915TMP-TYOKI                 PIC  X(01).
           03  SKT2915TMP-HKNJANAME                 PIC  X(20).
      *
           03  SKT2915TMP-HKN-AREA  OCCURS  2.
               05  SKT2915TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT2915TMP-KENSU             PIC  9(05).
                   07  SKT2915TMP-NISSU             PIC  9(05).
                   07  SKT2915TMP-TENSU             PIC  9(09).
                   07  SKT2915TMP-ITBFTN            PIC  9(09).
               05  SKT2915TMP-S-KENSU               PIC  9(05).
               05  SKT2915TMP-S-NISSU               PIC  9(05).
               05  SKT2915TMP-S-KINGK               PIC  9(09).
               05  SKT2915TMP-S-FTNGK               PIC  9(09).
           03  SKT2915TMP-KOH-AREA  OCCURS  20.
               05  SKT2915TMP-KOHNUM                PIC  X(03).
               05  SKT2915TMP-KOHNAME               PIC  X(10).
               05  SKT2915TMP-KOH-NYUGAI-TBL OCCURS 2.
                   07  SKT2915TMP-KKENSU            PIC  9(05).
                   07  SKT2915TMP-KNISSU            PIC  9(05).
                   07  SKT2915TMP-KTENSU            PIC  9(09).
                   07  SKT2915TMP-KITBFTN           PIC  9(09).
               05  SKT2915TMP-S-KKENSU              PIC  9(05).
               05  SKT2915TMP-S-KNISSU              PIC  9(05).
               05  SKT2915TMP-S-KKINGK              PIC  9(09).
               05  SKT2915TMP-S-KFTNGK              PIC  9(09).
           03  SKT2915TMP-TKENSU                    PIC  9(05).
