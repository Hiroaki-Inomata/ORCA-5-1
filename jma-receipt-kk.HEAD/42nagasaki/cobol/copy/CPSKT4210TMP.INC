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
      *    診療報酬請求書     COPY (CPSKT4210TMP.INC)            *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者    日付        内容
      * 01.00.01     門間      2008/11/20  シス管「2007」公費まとめ対応
      * 01.00.02     萩野      2019/02/07  修正履歴コメント削除
      ************************************************************
       01  SKT4210TMP-REC.
           03  SKT4210TMP-KEY.
               05  SKT4210TMP-SYUTURYOKU          PIC  9(01).
               05  SKT4210TMP-HKNJANUM            PIC  X(06).
               05  SKT4210TMP-SRYYM               PIC  9(06).
               05  SKT4210TMP-ZAITAKU             PIC  X(01).
               05  SKT4210TMP-KYURATE             PIC  9(03).
               05  SKT4210TMP-TYOKI               PIC  X(01).
           03  SKT4210TMP-PAGE-FLG1               PIC  9(01).
           03  SKT4210TMP-PAGE-FLG2               PIC  9(01).
           03  SKT4210TMP-PAGE-FLG3               PIC  9(01).
           03  SKT4210TMP-TBL             OCCURS  13.
               05  SKT4210TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT4210TMP-KENSU               PIC  9(06).
                   07  SKT4210TMP-NISSU               PIC  9(06).
                   07  SKT4210TMP-TENSU               PIC  9(10).
                   07  SKT4210TMP-YKZFTN              PIC  9(10).
                   07  SKT4210TMP-ITBFTN              PIC  9(10).
               05  SKT4210TMP-SKENSU                  PIC  9(06).
               05  SKT4210TMP-SNISSU                  PIC  9(05).
               05  SKT4210TMP-SKINGK                  PIC  9(10).
               05  SKT4210TMP-SFTNGK                  PIC  9(10).
           03  SKT4210TMP-KOH-AREA.
               05  SKT4210TMP-KOH-TBL  OCCURS  20.
                   07  SKT4210TMP-KOHNUM              PIC  X(03).
                   07  SKT4210TMP-KOHNAME             PIC  X(08).
                   07  SKT4210TMP-KOH-NYUGAI-TBL OCCURS 2.
                       09  SKT4210TMP-K-KENSU         PIC  9(05).
                       09  SKT4210TMP-K-NISSU         PIC  9(05).
                       09  SKT4210TMP-K-TENSU         PIC  9(09).
                       09  SKT4210TMP-K-YKZFTN        PIC  9(09).
                       09  SKT4210TMP-K-ITBFTN        PIC  9(09).
                   07  SKT4210TMP-K-SKENSU            PIC  9(05).
                   07  SKT4210TMP-K-SNISSU            PIC  9(05).
                   07  SKT4210TMP-K-SKINGK            PIC  9(09).
                   07  SKT4210TMP-K-SFTNGK            PIC  9(09).
           03  SKT4210TMP-RKOH-AREA.
               05  SKT4210TMP-RKOH-TBL  OCCURS  20.
                   07  SKT4210TMP-RKOHNUM              PIC  X(03).
                   07  SKT4210TMP-RKOHNAME             PIC  X(08).
                   07  SKT4210TMP-RKOH-NYUGAI-TBL OCCURS 2.
                       09  SKT4210TMP-RK-KENSU         PIC  9(05).
                       09  SKT4210TMP-RK-NISSU         PIC  9(05).
                       09  SKT4210TMP-RK-TENSU         PIC  9(09).
                       09  SKT4210TMP-RK-YKZFTN        PIC  9(09).
                       09  SKT4210TMP-RK-ITBFTN        PIC  9(09).
                   07  SKT4210TMP-RK-SKENSU            PIC  9(05).
                   07  SKT4210TMP-RK-SNISSU            PIC  9(05).
                   07  SKT4210TMP-RK-SKINGK            PIC  9(09).
                   07  SKT4210TMP-RK-SFTNGK            PIC  9(09).
           03  SKT4210TMP-GKOH-AREA.
               05  SKT4210TMP-GKOH-TBL  OCCURS  2.
                   07  SKT4210TMP-GKOH-NYUGAI-TBL OCCURS 2.
                       09  SKT4210TMP-G-KENSU         PIC  9(05).
                       09  SKT4210TMP-G-NISSU         PIC  9(05).
                       09  SKT4210TMP-G-TENSU         PIC  9(09).
                       09  SKT4210TMP-G-ITBFTN        PIC  9(09).
                   07  SKT4210TMP-G-SKENSU            PIC  9(05).
                   07  SKT4210TMP-G-SNISSU            PIC  9(05).
                   07  SKT4210TMP-G-SKINGK            PIC  9(09).
                   07  SKT4210TMP-G-SFTNGK            PIC  9(09).
