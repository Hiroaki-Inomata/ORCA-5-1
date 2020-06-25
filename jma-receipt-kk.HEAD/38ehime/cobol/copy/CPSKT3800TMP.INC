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
       01  SKT3800TMP-REC.
           03  SKT3800TMP-KEY.
               05  SKT3800TMP-PREFKBN             PIC  X(01).
           03  SKT3800TMP-SEIKYU-TBL     OCCURS  9.
               05  SKT3800TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT3800TMP-KENSU            PIC  9(06).
                   07  SKT3800TMP-TENSU            PIC  9(10).
                   07  SKT3800TMP-ITBFTN           PIC  9(10).
               05  SKT3800TMP-S-KENSU              PIC  9(06).
               05  SKT3800TMP-S-KINGAKU            PIC  9(10).
               05  SKT3800TMP-S-FTNGAKU            PIC  9(10).
           03  SKT3800TMP-RSEIKYU-TBL     OCCURS  2.
               05  SKT3800TMP-RNYUGAI-TBL  OCCURS  2.
                   07  SKT3800TMP-RKENSU            PIC  9(06).
                   07  SKT3800TMP-RTENSU            PIC  9(10).
                   07  SKT3800TMP-RITBFTN           PIC  9(10).
               05  SKT3800TMP-S-RKENSU              PIC  9(06).
               05  SKT3800TMP-S-RKINGAKU            PIC  9(10).
               05  SKT3800TMP-S-RFTNGAKU            PIC  9(10).
           03  SKT3800TMP-KSEIKYU-TBL     OCCURS  2.
               05  SKT3800TMP-KNYUGAI-TBL  OCCURS  2.
                   07  SKT3800TMP-KKENSU            PIC  9(06).
                   07  SKT3800TMP-KTENSU            PIC  9(10).
                   07  SKT3800TMP-KITBFTN           PIC  9(10).
               05  SKT3800TMP-S-KKENSU              PIC  9(06).
               05  SKT3800TMP-S-KKINGAKU            PIC  9(10).
               05  SKT3800TMP-S-KFTNGAKU            PIC  9(10).
           03  SKT3800TMP-TSEIKYU-TBL.
               05  SKT3800TMP-TNYUGAI-TBL  OCCURS  2.
                   07  SKT3800TMP-TKENSU            PIC  9(06).
                   07  SKT3800TMP-TTENSU            PIC  9(10).
                   07  SKT3800TMP-TITBFTN           PIC  9(10).
           03  SKT3800TMP-KENNAI-FLG   OCCURS 28    PIC  9(01).
           03  SKT3800TMP-KENNAI-KENSU              PIC  9(02).
           03  SKT3800TMP-KENNAI-HKNJA-OLD OCCURS 50    PIC  X(08).
           03  SKT3800TMP-KENGAI-HKNJA     OCCURS 100   PIC  X(08).
           03  SKT3800TMP-KENGAI-KENSU              PIC  9(03).
           03  SKT3800TMP-HKN-TBL OCCURS 2.
               05  SKT3800TMP-NYUGAI-TBL OCCURS 2.
                   07  SKT3800TMP-GKENSU            PIC 9(06).
                   07  SKT3800TMP-GTENSU            PIC 9(10).
