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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    有村     03.03.07  新規
      ************************************************************
       01  SKT1100.
           03  SKT00-SRYYM                   PIC  X(16).
           03  SKT00-HOSPCDN                 PIC  X(40).
           03  SKT00-HOSP1.
               05  SKT00-HOSPADRS1           PIC  X(100).
               05  SKT00-HOSPNAME1           PIC  X(100).
               05  SKT00-KAISETUNAME1        PIC  X(80).
               05  SKT00-TEL1                PIC  X(15).
               05  SKT00-FAX1                PIC  X(15).
           03  SKT00-HOSP2.
               05  SKT00-HOSPADRS2           PIC  X(100).
               05  SKT00-HOSPNAME2           PIC  X(100).
               05  SKT00-KAISETUNAME2        PIC  X(80).
               05  SKT00-TEL2                PIC  X(15).
               05  SKT00-FAX2                PIC  X(15).
      *
           03  SKT00-HEIKIN                  PIC  X(09).
      *    帳票下部情報
           03  SKT00-LSRYYM                  PIC  X(02).
           03  SKT00-LHOSPCD                 PIC  X(07).
           03  SKT00-LSEIKYUGK               PIC  X(09).
      *    老人
           03  SKT00-RJN-TBL.
               05  SKT00-RKENSU               PIC  X(05).
               05  SKT00-RNISSU               PIC  X(05).
               05  SKT00-RTENSU               PIC  X(09).
               05  SKT00-RSEIKYUGK            PIC  X(10).
      *    退職
           03  SKT00-TAISHOKU-TBL.
               05  SKT00-TKENSU               PIC  X(05).
               05  SKT00-TNISSU               PIC  X(05).
               05  SKT00-TTENSU               PIC  X(09).
               05  SKT00-TSEIKYUGK            PIC  X(10).
      *    一般国保・公費
           03  SKT00-SEIKYU-TBL   OCCURS 17.
               05  SKT00-HKNKOHNAME.
                   07  SKT00-NAME            PIC  X(20).
               05  SKT00-WARI                PIC  X(06).
               05  SKT00-KENSU               PIC  X(05).
               05  SKT00-NISSU               PIC  X(05).
               05  SKT00-TENSU               PIC  X(09).
               05  SKT00-SEIKYUGK            PIC  X(10).
      *    合計
           03  SKT00-GOKEI-TBL.
               05  SKT00-GKENSU               PIC  X(05).
               05  SKT00-GNISSU               PIC  X(05).
               05  SKT00-GTENSU               PIC  X(09).
               05  SKT00-GSEIKYUGK            PIC  X(10).
