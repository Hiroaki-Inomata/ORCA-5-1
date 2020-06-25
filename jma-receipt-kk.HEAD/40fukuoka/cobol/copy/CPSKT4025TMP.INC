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
      *    診療報酬請求書     COPY (CPSKT4025TMP.INC)              *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         10/06/10  原爆用法別番号追加
      *  01.00.02    門間         19/01/10  修正履歴コメント削除
      ************************************************************
       01  SKT4025TMP-REC.
           03  SKT4025TMP-KEY.
               05  SKT4025TMP-SYUTURYOKU            PIC  9(01).
               05  SKT4025TMP-HKNJANUM              PIC  X(08).
               05  SKT4025TMP-ZAITAKU               PIC  X(01).
               05  SKT4025TMP-GENMEN                PIC  X(01).
      *
           03  SKT4025TMP-HKN-AREA  OCCURS  2.
               05  SKT4025TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT4025TMP-KENSU             PIC  9(05).
                   07  SKT4025TMP-NISSU             PIC  9(05).
                   07  SKT4025TMP-TENSU             PIC  9(09).
                   07  SKT4025TMP-ITBFTN            PIC  9(09).
               05  SKT4025TMP-S-KENSU               PIC  9(05).
               05  SKT4025TMP-S-NISSU               PIC  9(05).
               05  SKT4025TMP-S-KINGK               PIC  9(13).
               05  SKT4025TMP-S-FTNGK               PIC  9(09).
           03  SKT4025TMP-KOH-AREA  OCCURS  20.
               05  SKT4025TMP-KOHNUM                PIC  X(03).
               05  SKT4025TMP-KOHNAME               PIC  X(10).
               05  SKT4025TMP-KOH-NYUGAI-TBL OCCURS 2.
                   07  SKT4025TMP-KKENSU            PIC  9(05).
                   07  SKT4025TMP-KNISSU            PIC  9(05).
                   07  SKT4025TMP-KTENSU            PIC  9(09).
                   07  SKT4025TMP-KYKZFTN           PIC  9(09).
                   07  SKT4025TMP-KITBFTN           PIC  9(09).
                   07  SKT4025TMP-KPTFTN            PIC  9(09).
               05  SKT4025TMP-S-KKENSU              PIC  9(05).
               05  SKT4025TMP-S-KNISSU              PIC  9(05).
               05  SKT4025TMP-S-KKINGK              PIC  9(13).
               05  SKT4025TMP-S-KFTNGK              PIC  9(09).
           03  SKT4025TMP-GENBAKU-AREA.
               05  SKT4025TMP-GKOHNUM               PIC  X(02).
               05  SKT4025TMP-KOH-NYUGAI-TBL OCCURS 2.
                   07  SKT4025TMP-GKENSU            PIC  9(05).
                   07  SKT4025TMP-GNISSU            PIC  9(05).
                   07  SKT4025TMP-GTENSU            PIC  9(09).
                   07  SKT4025TMP-GYKZFTN           PIC  9(09).
                   07  SKT4025TMP-GITBFTN           PIC  9(09).
                   07  SKT4025TMP-GPTFTN            PIC  9(09).
               05  SKT4025TMP-S-GKENSU              PIC  9(05).
               05  SKT4025TMP-S-GNISSU              PIC  9(05).
               05  SKT4025TMP-S-GKINGK              PIC  9(13).
               05  SKT4025TMP-S-GFTNGK              PIC  9(09).
