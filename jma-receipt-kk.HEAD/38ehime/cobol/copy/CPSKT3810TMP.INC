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
      * Maj/Min/Rev  修正者    日付        内容
      *  01.00.01    長屋      08/04/29    帳票様式に伴う修正
      *  01.00.02    門間      18/09/05    不要コメントを削除
      ************************************************************
       01  SKT3810TMP-REC.
           03  SKT3810TMP-KEY.
               05  SKT3810TMP-SYUTURYOKU          PIC  9(01).
               05  SKT3810TMP-HKNJANUM            PIC  X(06).
               05  SKT3810TMP-SRYYM               PIC  9(06).
               05  SKT3810TMP-ZAISOU              PIC  X(01).
               05  SKT3810TMP-KYURATE             PIC  X(03).
           03  SKT3810TMP-PAGE-FLG1               PIC  9(01).
           03  SKT3810TMP-PAGE-FLG2               PIC  9(01).
           03  SKT3810TMP-SEIKYU-TBL     OCCURS  9.
               05  SKT3810TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT3810TMP-KENSU            PIC  9(06).
                   07  SKT3810TMP-NISSU            PIC  9(06).
                   07  SKT3810TMP-TENSU            PIC  9(10).
                   07  SKT3810TMP-ITBFTN           PIC  9(10).
               05  SKT3810TMP-S-KENSU              PIC  9(06).
               05  SKT3810TMP-S-NISSU              PIC  9(06).
               05  SKT3810TMP-S-KINGAKU            PIC  9(10).
               05  SKT3810TMP-S-FTNGAKU            PIC  9(10).
           03  SKT3810TMP-RSEIKYU-TBL     OCCURS  2.
               05  SKT3810TMP-RNYUGAI-TBL  OCCURS  2.
                   07  SKT3810TMP-RKENSU            PIC  9(06).
                   07  SKT3810TMP-RNISSU            PIC  9(06).
                   07  SKT3810TMP-RTENSU            PIC  9(10).
                   07  SKT3810TMP-RITBFTN           PIC  9(10).
               05  SKT3810TMP-S-RKENSU              PIC  9(06).
               05  SKT3810TMP-S-RNISSU              PIC  9(06).
               05  SKT3810TMP-S-RKINGAKU            PIC  9(10).
               05  SKT3810TMP-S-RFTNGAKU            PIC  9(10).
           03  SKT3810TMP-KSEIKYU-TBL     OCCURS  2.
               05  SKT3810TMP-KNYUGAI-TBL  OCCURS  2.
                   07  SKT3810TMP-KKENSU            PIC  9(06).
                   07  SKT3810TMP-KNISSU            PIC  9(06).
                   07  SKT3810TMP-KTENSU            PIC  9(10).
                   07  SKT3810TMP-KITBFTN           PIC  9(10).
               05  SKT3810TMP-S-KKENSU              PIC  9(06).
               05  SKT3810TMP-S-KNISSU              PIC  9(06).
               05  SKT3810TMP-S-KKINGAKU            PIC  9(10).
               05  SKT3810TMP-S-KFTNGAKU            PIC  9(10).
           03  SKT3810TMP-TSEIKYU-TBL.
               05  SKT3810TMP-TNYUGAI-TBL  OCCURS  2.
                   07  SKT3810TMP-TKENSU            PIC  9(06).
                   07  SKT3810TMP-TNISSU            PIC  9(06).
                   07  SKT3810TMP-TTENSU            PIC  9(10).
                   07  SKT3810TMP-TITBFTN           PIC  9(10).
