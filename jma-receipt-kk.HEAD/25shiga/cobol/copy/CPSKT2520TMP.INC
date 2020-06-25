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
      *    ¿ÇÎÅÊó½·ÀÁµá½ñ     COPY (CPSKT0710TMP.INC)              *
      *
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô  ÆüÉÕ      ÆâÍÆ
      *  01.00.01    µÈÀî    08/10/20  ¥·¥¹´É¡Ö2007¡×¸øÈñ¤Þ¤È¤áÂÐ±þ
      *  01.00.02    °æ¾å    18/12/12  ½¤ÀµÍúÎò¥³¥á¥ó¥Èºï½ü
      ************************************************************
       01  SKT2520TMP-REC.
           03  SKT2520TMP-KEY.
               05  SKT2520TMP-SYUTURYOKU           PIC  9(02).
               05  SKT2520TMP-HKNJANUM             PIC  X(06).
               05  SKT2520TMP-HKNJANAME            PIC  X(16).
               05  SKT2520TMP-ZAITAKU              PIC  X(01).
               05  SKT2520TMP-KYURATE              PIC  9(03).
           03  SKT2520TMP-SEIKYU-TBL     OCCURS  11.
               05  SKT2520TMP-KENSU            PIC  9(06).
               05  SKT2520TMP-NISSU            PIC  9(06).
               05  SKT2520TMP-TENSU            PIC  9(10).
               05  SKT2520TMP-YKZFTN           PIC  9(10).
               05  SKT2520TMP-ITBFTN           PIC  9(10).
           03  SKT2520TMP-FUKUSI-TBL.
               05  SKT2520TMP-FKENSU            PIC  9(06).
               05  SKT2520TMP-FNISSU            PIC  9(06).
               05  SKT2520TMP-FTENSU            PIC  9(10).
               05  SKT2520TMP-FYKZFTN           PIC  9(10).
               05  SKT2520TMP-FITBFTN           PIC  9(10).
           03  SKT2520TMP-KOH-AREA.
               05  SKT2520TMP-KOH-TBL      OCCURS  20.
                   07  SKT2520TMP-KOHNUM       PIC  X(03).
                   07  SKT2520TMP-KOHNAME      PIC  X(12).
                   07  SKT2520TMP-KKENSU       PIC  9(06).
                   07  SKT2520TMP-KNISSU       PIC  9(06).
                   07  SKT2520TMP-KTENSU       PIC  9(10).
                   07  SKT2520TMP-KYKZFTN      PIC  9(10).
                   07  SKT2520TMP-KITBFTN      PIC  9(10).
           03  SKT2520TMP-TYOKI-KENSU          PIC  9(04).
           03  SKT2520TMP-GENBK-KENSU          PIC  9(04).
