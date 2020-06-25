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
      *    診療報酬請求書     COPY (CPSKT1310TMP.INC)              *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付      内容
      *  01.00.01    吉川    09/01/26  公費名称追加
      *  01.00.02    吉川    11/03/15  東北地方太平洋沖地震対応（対象者別集計）
      *  01.00.03    門間    18/11/02  修正履歴コメント削除
      ************************************************************
       01  SKT1310TMP-REC.
           03  SKT1310TMP-KEY.
               05  SKT1310TMP-SYUTURYOKU           PIC  9(01).
               05  SKT1310TMP-NYUGAIKBN            PIC  X(01).
               05  SKT1310TMP-HKNJANUM             PIC  X(06).
               05  SKT1310TMP-ZAITAKU              PIC  X(01).
               05  SKT1310TMP-GENMEN               PIC  X(05).
           03  SKT1310TMP-KYURATE                  PIC  9(03).
           03  SKT1310TMP-SEIKYU-TBL     OCCURS  12.
               05  SKT1310TMP-KENSU                PIC  9(06).
               05  SKT1310TMP-NISSU                PIC  9(06).
               05  SKT1310TMP-TENSU                PIC  9(10).
               05  SKT1310TMP-ITBFTN               PIC  9(10).
               05  SKT1310TMP-S-KENSU              PIC  9(06).
               05  SKT1310TMP-S-NISSU              PIC  9(06).
               05  SKT1310TMP-S-KINGK              PIC  9(10).
               05  SKT1310TMP-S-FTNGK              PIC  9(10).
           03  SKT1310TMP-KOH-AREA.
               05  SKT1310TMP-KOH-TBL      OCCURS  20.
                   07  SKT1310TMP-KOHNUM           PIC  X(03).
                   07  SKT1310TMP-KOHNAME          PIC  X(10).
                   07  SKT1310TMP-KKENSU           PIC  9(06).
                   07  SKT1310TMP-KNISSU           PIC  9(06).
                   07  SKT1310TMP-KTENSU           PIC  9(10).
                   07  SKT1310TMP-KITBFTN          PIC  9(10).
                   07  SKT1310TMP-S-KKENSU         PIC  9(06).
                   07  SKT1310TMP-S-KNISSU         PIC  9(06).
                   07  SKT1310TMP-S-KKINGK         PIC  9(10).
                   07  SKT1310TMP-S-KFTNGK         PIC  9(10).
