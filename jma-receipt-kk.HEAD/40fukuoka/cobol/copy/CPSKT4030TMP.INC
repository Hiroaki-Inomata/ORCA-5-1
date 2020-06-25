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
      *    特別療養費診療明細書     COPY (CPSKT4030TMP.INC)
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
       01  SKT4030TMP-REC.
           03  SKT4030TMP-KEY.
               05  SKT4030TMP-SYUTURYOKU            PIC  9(01).
               05  SKT4030TMP-HKNJANUM              PIC  X(06).
               05  SKT4030TMP-ZAITAKU               PIC  X(01).
               05  SKT4030TMP-GENMEN                PIC  X(01).
           03  SKT4030TMP-HKN-AREA  OCCURS  5.
               05  SKT4030TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT4030TMP-KENSU             PIC  9(05).
                   07  SKT4030TMP-NISSU             PIC  9(05).
                   07  SKT4030TMP-TENSU             PIC  9(09).
                   07  SKT4030TMP-ITBFTN            PIC  9(09).
               05  SKT4030TMP-S-KENSU               PIC  9(05).
               05  SKT4030TMP-S-NISSU               PIC  9(05).
               05  SKT4030TMP-S-KINGK               PIC  9(13).
               05  SKT4030TMP-S-FTNGK               PIC  9(09).
