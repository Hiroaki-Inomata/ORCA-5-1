      *******************************************************************
      * Project code name "ORCA"
      * Æü°åÉ¸½à¥ì¥»¥×¥È¥½¥Õ¥È¡ÊJMA standard receipt software¡Ë
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
      *    ¿ÇÎÅÊó½·ÀÁµá½ñ     COPY (CPSKT2110TMP.INC)            *
      *
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    °æ¾å      2019/01/10   ½¤ÀµÍúÎò¥³¥á¥ó¥Èºï½ü
      ************************************************************
       01  SKT3210TMP-REC.
           03  SKT3210TMP-KEY.
               05  SKT3210TMP-SYUTURYOKU          PIC  9(01).
               05  SKT3210TMP-HKNJANUM            PIC  X(06).
               05  SKT3210TMP-KENGAITAHO          PIC  9(01).
               05  SKT3210TMP-ZAI-FLG             PIC  X(01).
           03  SKT3210TMP-IPPAN-TBL  OCCURS  9.
               05  SKT3210TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT3210TMP-KENSU         PIC  9(06).
                   07  SKT3210TMP-NISSU         PIC  9(06).
                   07  SKT3210TMP-TENSU         PIC  9(10).
                   07  SKT3210TMP-YKZFTN        PIC  9(10).
                   07  SKT3210TMP-ITBFTN        PIC  9(07).
                   07  SKT3210TMP-TYOUKEN       PIC  9(03).
               05  SKT3210TMP-S-KENSU           PIC  9(06).
               05  SKT3210TMP-S-NISSU           PIC  9(06).
               05  SKT3210TMP-S-KINGAKU         PIC  9(10).
               05  SKT3210TMP-S-FTNGAKU         PIC  9(10).
           03  SKT3210TMP-KOH-TBL  OCCURS  20.
               05  SKT3210TMP-KOHNUM            PIC  X(03).
               05  SKT3210TMP-KOHNAME           PIC  X(08).
               05  SKT3210TMP-KKENSU            PIC  9(06).
               05  SKT3210TMP-KNISSU            PIC  9(06).
               05  SKT3210TMP-KTENSU            PIC  9(10).
               05  SKT3210TMP-KYKZFTN           PIC  9(10).
               05  SKT3210TMP-KNJFTN            PIC  9(10).
               05  SKT3210TMP-S-KKENSU          PIC  9(06).
               05  SKT3210TMP-S-KKINGAKU        PIC  9(10).
               05  SKT3210TMP-S-KFTNGAKU        PIC  9(10).
           03  SKT3210TMP-RJN-TBL  OCCURS  2.
               05  SKT3210TMP-R-NYUGAI-TBL  OCCURS  2.
                   07  SKT3210TMP-RKENSU        PIC  9(06).
                   07  SKT3210TMP-RNISSU        PIC  9(06).
                   07  SKT3210TMP-RTENSU        PIC  9(10).
                   07  SKT3210TMP-RYKZFTN       PIC  9(10).
                   07  SKT3210TMP-RITBFTN       PIC  9(10).
                   07  SKT3210TMP-RTYOUKEN      PIC  9(03).
                   07  SKT3210TMP-RGENKEN       PIC  9(03).
               05  SKT3210TMP-S-RKENSU          PIC  9(06).
               05  SKT3210TMP-S-RNISSU          PIC  9(06).
               05  SKT3210TMP-S-RKINGAKU        PIC  9(10).
               05  SKT3210TMP-S-RFTNGAKU        PIC  9(10).
