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
      *    診療報酬請求書     COPY (CPSKT2410TMP.INC)            *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SKT2410TMP-REC.
           03      SKT2410TMP-KEY.
             05    SKT2410TMP-PREFKBN             PIC  X(01).
             05    SKT2410TMP-KBN                 PIC  X(01).
             05    SKT2410TMP-SYUTURYOKU          PIC  X(01).
             05    SKT2410TMP-HKNJANUM            PIC  X(06).
             05    SKT2410TMP-WARIAI              PIC  9(02).
           03      SKT2410TMP-IPPAN               OCCURS  5.
             05    SKT2410TMP-NYUGAI-TBL          OCCURS  2.
               07  SKT2410TMP-KENSU               PIC  9(06).
               07  SKT2410TMP-NISSU               PIC  9(05).
               07  SKT2410TMP-TENSU               PIC  9(10).
               07  SKT2410TMP-ITBFTN              PIC  9(10).
             05    SKT2410TMP-S-KENSU             PIC  9(06).
             05    SKT2410TMP-S-NISSU             PIC  9(05).
             05    SKT2410TMP-S-KINGK             PIC  9(10).
             05    SKT2410TMP-S-FTNGK             PIC  9(10).
           03      SKT2410TMP-KOHI                PIC  9(05).
           03      SKT2410TMP-GENBAKU             PIC  9(05).
           03      SKT2410TMP-ZAITAKU             PIC  9(05).
