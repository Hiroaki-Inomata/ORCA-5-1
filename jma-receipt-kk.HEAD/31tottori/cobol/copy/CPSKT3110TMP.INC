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
      *    診療報酬請求書     COPY (CPSKT3110TMP.INC)            *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SKT3110TMP-REC.
           03  SKT3110TMP-KEY.
               05  SKT3110TMP-SYUTURYOKU             PIC  9(01).
               05  SKT3110TMP-HKNJANUM               PIC  X(06).
               05  SKT3110TMP-SRYYM                  PIC  9(06).
           03  SKT3110TMP-KYURATE                 PIC  9(03).
           03  SKT3110TMP-ZAITAKU                     PIC  X(01).
      *     03  SKT3110TMP-PAGE-FLG2                   PIC  9(01).
           03  SKT3110TMP-KOH-CNT                     PIC  9(02).
           03  SKT3110TMP-TBL                         OCCURS  11.
               05  SKT3110TMP-NYUGAI-TBL              OCCURS  2.
                   07  SKT3110TMP-KENSU               PIC  9(05).
                   07  SKT3110TMP-NISSU               PIC  9(05).
                   07  SKT3110TMP-TENSU               PIC  9(09).
                   07  SKT3110TMP-YKZFTN              PIC  9(07).
                   07  SKT3110TMP-ITBFTN              PIC  9(09).
               05  SKT3110TMP-SKENSU                  PIC  9(05).
               05  SKT3110TMP-SNISSU                  PIC  9(05).
               05  SKT3110TMP-SKINGK                  PIC  9(09).
               05  SKT3110TMP-SFTNGK                  PIC  9(09).
           03  SKT3110TMP-GENBKKEN                    PIC  9(05).
           03  SKT3110TMP-KOH-AREA.
               05  SKT3110TMP-KOH-TBL                 OCCURS  20.
                   07  SKT3110TMP-KOHNUM              PIC  X(03).
                   07  SKT3110TMP-KOHNAME             PIC  X(08).
                   07  SKT3110TMP-KOH-NYUGAI-TBL      OCCURS 2.
                       09  SKT3110TMP-KKENSU          PIC  9(05).
                       09  SKT3110TMP-KNISSU          PIC  9(05).
                       09  SKT3110TMP-KTENSU          PIC  9(09).
                       09  SKT3110TMP-KYKZFTN         PIC  9(07).
                       09  SKT3110TMP-KITBFTN         PIC  9(09).
                   07  SKT3110TMP-K-SKENSU            PIC  9(05).
                   07  SKT3110TMP-K-SNISSU            PIC  9(05).
                   07  SKT3110TMP-K-SKINGK            PIC  9(09).
                   07  SKT3110TMP-K-SFTNGK            PIC  9(09).
           03  ZAITAKU-TBL                            OCCURS  6.
                   05  SKT3110TMP-ZKENSU               PIC  9(05).
                   05  SKT3110TMP-ZNISSU               PIC  9(05).
                   05  SKT3110TMP-ZTENSU               PIC  9(09).
                   05  SKT3110TMP-ZITBFTN              PIC  9(09).

