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
      *    総括表一時ファイル           CPSKT3000TMP.INC           *
      ************************************************************
       01  SKT3000TMP-REC.
           03  SKT3000TMP-KEY.
               05  SKT3000TMP-KBN                PIC  9(01).
               05  SKT3000TMP-SYUTURYOKU         PIC  9(01).
               05  SKT3000TMP-NYUGAIKBN          PIC  9(01).
               05  SKT3000TMP-HKNJANUM           PIC  X(06).
               05  SKT3000TMP-ZAITAKU            PIC  X(01).
               05  SKT3000TMP-MARUCHO            PIC  9(01).
           03  SKT3000TMP-PAGE-FLG1              PIC  9(01).
           03  SKT3000TMP-PAGE-FLG2              PIC  9(01).
           03  SKT3000TMP-IPAN-TBL     OCCURS 11.
               05  SKT3000-NYUGAI-TBL     OCCURS  2.
                   07  SKT3000TMP-KENSU          PIC  9(05).
                   07  SKT3000TMP-NISSU          PIC  9(05).
                   07  SKT3000TMP-TENSU          PIC  9(09).
                   07  SKT3000TMP-ITBFTN         PIC  9(09).
               05  SKT3000TMP-SKENSU             PIC  9(05).
               05  SKT3000TMP-SNISSU             PIC  9(05).
               05  SKT3000TMP-SKINGK             PIC  9(09).
               05  SKT3000TMP-SFTNGK             PIC  9(09).
           03  SKT3000TMP-KOH-AREA.
               05  SKT3000TMP-KOH-TBL     OCCURS  20.
                   07  SKT3000TMP-KOHNUM             PIC  X(03).
                   07  SKT3000TMP-KOHNAME            PIC  X(10).
                   07  SKT3000TMP-K-NYUGAI-TBL     OCCURS  2.
                       09  SKT3000TMP-K-KENSU        PIC  9(05).
                       09  SKT3000TMP-K-NISSU        PIC  9(05).
                       09  SKT3000TMP-K-TENSU        PIC  9(09).
                       09  SKT3000TMP-K-ITBFTN       PIC  9(09).
                   07  SKT3000TMP-K-SKENSU           PIC  9(05).
                   07  SKT3000TMP-K-SNISSU           PIC  9(05).
                   07  SKT3000TMP-K-SKINGK           PIC  9(09).
                   07  SKT3000TMP-K-SFTNGK           PIC  9(09).
           03  SKT3000TMP-RKOH-AREA.
               05  SKT3000TMP-RKOH-TBL     OCCURS  20.
                   07  SKT3000TMP-RKOHNUM             PIC  X(03).
                   07  SKT3000TMP-RKOHNAME            PIC  X(10).
                   07  SKT3000TMP-K-RNYUGAI-TBL     OCCURS  2.
                       09  SKT3000TMP-K-RKENSU        PIC  9(05).
                       09  SKT3000TMP-K-RNISSU        PIC  9(05).
                       09  SKT3000TMP-K-RTENSU        PIC  9(09).
                       09  SKT3000TMP-K-RITBFTN       PIC  9(09).
                   07  SKT3000TMP-K-RSKENSU           PIC  9(05).
                   07  SKT3000TMP-K-RSNISSU           PIC  9(05).
                   07  SKT3000TMP-K-RSKINGK           PIC  9(09).
                   07  SKT3000TMP-K-RSFTNGK           PIC  9(09).
           03  SKT3000TMP-TYOKI-GENBAKU-TBL   OCCURS  4.
               05  SKT3000TMP-TYOGEN             PIC  9(07).
           03  SKT3000TMP-GOKEI-TBL   OCCURS  2.
               05  SKT3000TMP-GKENSU                  PIC  9(05).
               05  SKT3000TMP-GKINGK                  PIC  9(07).
