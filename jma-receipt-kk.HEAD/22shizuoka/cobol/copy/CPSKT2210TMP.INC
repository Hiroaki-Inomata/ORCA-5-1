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
      *    総括表一時ファイル           CPSKT2210TMP.INC           *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者   日付       内容
      *  01.00.01    吉川     08/09/11   公費名称追加
      *  01.00.02    萩野     18/09/10   修正履歴コメント削除
      ************************************************************
       01  SKT2210TMP-REC.
           03  SKT2210TMP-KEY.
               05  SKT2210TMP-SYUTURYOKU         PIC  9(01).
               05  SKT2210TMP-HKNJANUM           PIC  X(06).
               05  SKT2210TMP-ZAITAKU            PIC  X(01).
               05  SKT2210TMP-SRYYM              PIC  9(06).
           03  SKT2210TMP-PAGE-FLG1              PIC  9(01).
           03  SKT2210TMP-PAGE-FLG2              PIC  9(01).
           03  SKT2210TMP-IPAN-TBL     OCCURS 11.
               05  SKT2210-NYUGAI-TBL     OCCURS  2.
                   07  SKT2210TMP-KENSU          PIC  9(05).
                   07  SKT2210TMP-NISSU          PIC  9(05).
                   07  SKT2210TMP-TENSU          PIC  9(09).
                   07  SKT2210TMP-ITBFTN         PIC  9(09).
               05  SKT2210TMP-SKENSU             PIC  9(05).
               05  SKT2210TMP-SNISSU             PIC  9(05).
               05  SKT2210TMP-SKINGK             PIC  9(09).
               05  SKT2210TMP-SFTNGK             PIC  9(09).
           03  SKT2210TMP-KOH-AREA.
               05  SKT2210TMP-KOH-TBL     OCCURS  20.
                   07  SKT2210TMP-KOHNUM             PIC  X(03).
                   07  SKT2210TMP-KOHNAME            PIC  X(08).
                   07  SKT2210TMP-K-NYUGAI-TBL     OCCURS  2.
                       09  SKT2210TMP-K-KENSU        PIC  9(05).
                       09  SKT2210TMP-K-NISSU        PIC  9(05).
                       09  SKT2210TMP-K-TENSU        PIC  9(09).
                       09  SKT2210TMP-K-ITBFTN       PIC  9(09).
                   07  SKT2210TMP-K-SKENSU           PIC  9(05).
                   07  SKT2210TMP-K-SNISSU           PIC  9(05).
                   07  SKT2210TMP-K-SKINGK           PIC  9(09).
                   07  SKT2210TMP-K-SFTNGK           PIC  9(09).
           03  SKT2210TMP-RKOH-AREA.
               05  SKT2210TMP-RKOH-TBL     OCCURS  20.
                   07  SKT2210TMP-RKOHNUM             PIC  X(03).
                   07  SKT2210TMP-RKOHNAME            PIC  X(08).
                   07  SKT2210TMP-K-RNYUGAI-TBL     OCCURS  2.
                       09  SKT2210TMP-K-RKENSU        PIC  9(05).
                       09  SKT2210TMP-K-RNISSU        PIC  9(05).
                       09  SKT2210TMP-K-RTENSU        PIC  9(09).
                       09  SKT2210TMP-K-RITBFTN       PIC  9(09).
                   07  SKT2210TMP-K-RSKENSU           PIC  9(05).
                   07  SKT2210TMP-K-RSNISSU           PIC  9(05).
                   07  SKT2210TMP-K-RSKINGK           PIC  9(09).
                   07  SKT2210TMP-K-RSFTNGK           PIC  9(09).
           03  SKT2210TMP-TYOKI-GENBAKU-TBL   OCCURS  4.
               05  SKT2210TMP-TYOGEN             PIC  9(07).
           03  SKT2210TMP-GOKEI-TBL   OCCURS  2.
               05  SKT2210TMP-GKENSU                  PIC  9(05).
               05  SKT2210TMP-GKINGK                  PIC  9(07).
