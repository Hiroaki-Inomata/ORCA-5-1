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
      *    診療報酬請求書     COPY (CPSKT0710TMP.INC)              *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川      2013/11/05   月遅れ件数、長期件数追加
      *  01.00.02    萩野      2020/02/12   修正履歴コメント削除
      ************************************************************
       01  SKT2310TMP-REC.
           03  SKT2310TMP-KEY.
               05  SKT2310TMP-SYUTURYOKU           PIC  9(01).
               05  SKT2310TMP-HKNJANUM             PIC  X(06).
               05  SKT2310TMP-ZAISOU               PIC  X(01).
           03  SKT2310TMP-PAGE-FLG1                PIC  9(01).
           03  SKT2310TMP-PAGE-FLG2                PIC  9(01).
           03  SKT2310TMP-SEIKYU-TBL     OCCURS  10.
               05  SKT2310TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT2310TMP-KENSU            PIC  9(06).
                   07  SKT2310TMP-NISSU            PIC  9(06).
                   07  SKT2310TMP-TENSU            PIC  9(10).
                   07  SKT2310TMP-YKZFTN           PIC  9(10).
                   07  SKT2310TMP-ITBFTN           PIC  9(10).
               05  SKT2310TMP-S-KENSU              PIC  9(06).
               05  SKT2310TMP-S-NISSU              PIC  9(06).
               05  SKT2310TMP-S-KINGAKU            PIC  9(10).
               05  SKT2310TMP-S-FTNGAKU            PIC  9(10).
           03  SKT2310TMP-RSEIKYU-TBL     OCCURS  2.
               05  SKT2310TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT2310TMP-RKENSU            PIC  9(06).
                   07  SKT2310TMP-RNISSU            PIC  9(06).
                   07  SKT2310TMP-RTENSU            PIC  9(10).
                   07  SKT2310TMP-RYKZFTN           PIC  9(10).
                   07  SKT2310TMP-RITBFTN           PIC  9(10).
               05  SKT2310TMP-S-RKENSU              PIC  9(06).
               05  SKT2310TMP-S-RNISSU              PIC  9(06).
               05  SKT2310TMP-S-RKINGAKU            PIC  9(10).
               05  SKT2310TMP-S-RFTNGAKU            PIC  9(10).
           03  SKT02310TMP-KOHKENSU-TBL.
               05  SKT2310TMP-KOHKENSU1             PIC  9(07).
               05  SKT2310TMP-KOHKENSU2             PIC  9(07).
               05  SKT2310TMP-KOHKENSU3             PIC  9(07).
           03  SKT2310TMP-KOGAKU                    PIC  9(06).
           03  SKT2310TMP-TUKIOKURE                 PIC  9(06).
           03  SKT2310TMP-TYOKI                     PIC  9(06).
