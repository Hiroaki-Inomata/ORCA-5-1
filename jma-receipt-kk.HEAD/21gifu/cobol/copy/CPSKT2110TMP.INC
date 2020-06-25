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
      *    診療報酬請求書     COPY (CPSKT2110TMP.INC)            *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者   日付        内容
      * 01.00.01     門間     2008/11/20  シス管「2007」公費まとめ対応
      * 01.00.02     萩野     2018/10/11  修正履歴コメント削除
      ************************************************************
       01  SKT2110TMP-REC.
           03  SKT2110TMP-KEY.
               05  SKT2110TMP-SYUTURYOKU          PIC  9(01).
               05  SKT2110TMP-HKNJANUM            PIC  X(06).
               05  SKT2110TMP-KYURATE             PIC  9(03).
               05  SKT2110TMP-SYUBETU             PIC  9(01).
               05  SKT2110TMP-ZAITAKU             PIC  X(01).
               05  SKT2110TMP-SRYYMFLG            PIC  9(01).
           03  SKT2110TMP-IPPAN-TBL  OCCURS  5.
               05  SKT2110TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT2110TMP-KENSU         PIC  9(06).
                   07  SKT2110TMP-NISSU         PIC  9(06).
                   07  SKT2110TMP-TENSU         PIC  9(10).
                   07  SKT2110TMP-YKZFTN        PIC  9(10).
                   07  SKT2110TMP-ITBFTN        PIC  9(10).
               05  SKT2110TMP-S-KENSU           PIC  9(06).
               05  SKT2110TMP-S-NISSU           PIC  9(06).
               05  SKT2110TMP-S-KINGAKU         PIC  9(10).
               05  SKT2110TMP-S-FTNGAKU         PIC  9(10).
           03  SKT2110TMP-KOH-AREA.
               05  SKT2110TMP-KOH-TBL  OCCURS  20.
                   07  SKT2110TMP-KOHNUM            PIC  X(03).
                   07  SKT2110TMP-KOHNAME           PIC  X(08).
                   07  SKT2110TMP-K-NYUGAI-TBL  OCCURS  2.
                       09  SKT2110TMP-KKENSU        PIC  9(06).
                       09  SKT2110TMP-KNISSU        PIC  9(06).
                       09  SKT2110TMP-KTENSU        PIC  9(10).
                       09  SKT2110TMP-KYKZFTN       PIC  9(10).
                       09  SKT2110TMP-KITBFTN       PIC  9(10).
                   07  SKT2110TMP-S-KKENSU          PIC  9(06).
                   07  SKT2110TMP-S-KNISSU          PIC  9(06).
                   07  SKT2110TMP-S-KKINGAKU        PIC  9(10).
                   07  SKT2110TMP-S-KFTNGAKU        PIC  9(10).
           03  SKT2110TMP-TYOKI-TBL  OCCURS  2.
               05  SKT2110TMP-TKENSU            PIC  9(03).
           03  SKT2110TMP-TYOKIT-TBL  OCCURS  2.
               05  SKT2110TMP-TTKENSU           PIC  9(03).
           03  SKT2110TMP-RJN-TBL  OCCURS  2.
               05  SKT2110TMP-R-NYUGAI-TBL  OCCURS  2.
                   07  SKT2110TMP-RKENSU        PIC  9(06).
                   07  SKT2110TMP-RNISSU        PIC  9(06).
                   07  SKT2110TMP-RTENSU        PIC  9(10).
                   07  SKT2110TMP-RYKZFTN       PIC  9(10).
                   07  SKT2110TMP-RITBFTN       PIC  9(10).
               05  SKT2110TMP-S-RKENSU          PIC  9(06).
               05  SKT2110TMP-S-RNISSU          PIC  9(06).
               05  SKT2110TMP-S-RKINGAKU        PIC  9(10).
               05  SKT2110TMP-S-RFTNGAKU        PIC  9(10).
           03  SKT2110TMP-RKOH-AREA.
               05  SKT2110TMP-RKOH-TBL  OCCURS  20.
                   07  SKT2110TMP-RKOHNUM           PIC  X(03).
                   07  SKT2110TMP-RKOHNAME          PIC  X(08).
                   07  SKT2110TMP-RK-NYUGAI-TBL  OCCURS  2.
                       09  SKT2110TMP-RKKENSU       PIC  9(06).
                       09  SKT2110TMP-RKNISSU       PIC  9(06).
                       09  SKT2110TMP-RKTENSU       PIC  9(10).
                       09  SKT2110TMP-RKYKZFTN      PIC  9(10).
                       09  SKT2110TMP-RKITBFTN      PIC  9(10).
                   07  SKT2110TMP-S-RKKENSU         PIC  9(06).
                   07  SKT2110TMP-S-RKNISSU         PIC  9(06).
                   07  SKT2110TMP-S-RKKINGAKU       PIC  9(10).
                   07  SKT2110TMP-S-RKFTNGAKU       PIC  9(10).
           03  SKT2110TMP-RTYOKI-TBL  OCCURS  2.
               05  SKT2110TMP-RTKENSU           PIC  9(03).
           03  SKT2110TMP-RTYOKIT-TBL  OCCURS  2.
               05  SKT2110TMP-RTTKENSU          PIC  9(03).
           03  SKT2110TMP-GKENSU                PIC  9(03).
           03  SKT2110TMP-RGKENSU               PIC  9(03).
