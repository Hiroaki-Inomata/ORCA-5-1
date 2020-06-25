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
      *    診療報酬請求書     COPY (CPSKT4610TMP.INC)            *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付      内容
      *  01.00.01    吉川    08/10/17  シス管「2007」対応
      *  01.00.02    萩野    18/11/07  修正履歴コメント削除
      ************************************************************
       01  SKT1810TMP-REC.
           03  SKT1810TMP-KEY.
               05  SKT1810TMP-SYUTURYOKU          PIC  9(01).
               05  SKT1810TMP-HKNJANUM            PIC  X(06).
               05  SKT1810TMP-ZAITAKU             PIC  X(01).
           03  SKT1810TMP-KYURATE                 PIC  9(03) OCCURS 2.
           03  SKT1810TMP-KOH-CNT                 PIC  9(02).
           03  SKT1810TMP-TBL             OCCURS  12.
               05  SKT1810TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT1810TMP-KENSU               PIC  9(06).
                   07  SKT1810TMP-NISSU               PIC  9(06).
                   07  SKT1810TMP-TENSU               PIC  9(10).
                   07  SKT1810TMP-KESEKYUFU           PIC  9(10).
                   07  SKT1810TMP-ITBFTN              PIC  9(10).
               05  SKT1810TMP-SKENSU                  PIC  9(06).
               05  SKT1810TMP-SNISSU                  PIC  9(06).
               05  SKT1810TMP-SKINGK                  PIC  9(10).
               05  SKT1810TMP-SFTNGK                  PIC  9(10).
           03  SKT1810TMP-KESE-AREA.
               05  SKT1810TMP-KESE-KENSU              PIC  9(06).
               05  SKT1810TMP-KESE-NISSU              PIC  9(06).
               05  SKT1810TMP-KESE-TENSU              PIC  9(10).
           03  SKT1810TMP-KOH-AREA.
               05  SKT1810TMP-KOH-TBL  OCCURS  20.
                   07  SKT1810TMP-KOHNUM              PIC  X(03).
                   07  SKT1810TMP-KOHNAME             PIC  X(10).
                   07  SKT1810TMP-K-KENSU             PIC  9(06).
                   07  SKT1810TMP-K-NISSU             PIC  9(06).
                   07  SKT1810TMP-K-TENSU             PIC  9(09).
                   07  SKT1810TMP-K-ITBFTN            PIC  9(09).
                   07  SKT1810TMP-K-SKENSU            PIC  9(06).
                   07  SKT1810TMP-K-SNISSU            PIC  9(06).
                   07  SKT1810TMP-K-SKINGK            PIC  9(09).
                   07  SKT1810TMP-K-SFTNGK            PIC  9(09).

