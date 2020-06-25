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
      *    ¿ÇÎÅÊó½·ÀÁµá½ñ     COPY (CPSKT1410TMP.INC)              *
      *
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô       ÆüÉÕ      ÆâÍÆ
      *  01.00.01    µÈÀî         11/06/23  ÅìËÌÃÏÊýÂÀÊ¿ÍÎ²­ÃÏ¿ÌÂÐ±þ
      *  01.00.02    µÈÀî         15/01/30  Ê¿À®£²£·Ç¯£±·î²þÀµÂÐ±þ
      ************************************************************
       01  SKT1410TMP-REC.
           03  SKT1410TMP-KEY.
               05  SKT1410TMP-SYUTURYOKU           PIC  9(01).
               05  SKT1410TMP-HKNJANUM             PIC  X(06).
               05  SKT1410TMP-ZAITAKU              PIC  X(01).
      *=== ºÒ³²ÂÐ±þ by Yoshikawa(2011/06/23) start
               05  SKT1410TMP-GENMEN               PIC  X(05).
      *=== ºÒ³²ÂÐ±þ by Yoshikawa(2011/06/23) end
           03  SKT1410TMP-SEIKYU-TBL     OCCURS  15.
               05  SKT1410TMP-NYUGAI-TBL    OCCURS  2.
                   07  SKT1410TMP-KENSU            PIC  9(06).
                   07  SKT1410TMP-NISSU            PIC  9(06).
                   07  SKT1410TMP-TENSU            PIC  9(10).
      *             07  SKT1410TMP-YKZFTN           PIC  9(10).
                   07  SKT1410TMP-ITBFTN           PIC  9(10).
               05  SKT1410TMP-S-KENSU              PIC  9(06).
               05  SKT1410TMP-S-NISSU              PIC  9(06).
               05  SKT1410TMP-S-KINGAKU            PIC  9(10).
               05  SKT1410TMP-S-FTNGAKU            PIC  9(10).
           03  SKT1410TMP-KOH-TBL      OCCURS  34.
               05  SKT1410TMP-K-KENSU              PIC  9(06).
      *         05  SKT1410TMP-K-TENSU              PIC  9(10).
      *         05  SKT1410TMP-K-YKZFTN             PIC  9(10).
      *         05  SKT1410TMP-K-ITBFTN             PIC  9(10).
