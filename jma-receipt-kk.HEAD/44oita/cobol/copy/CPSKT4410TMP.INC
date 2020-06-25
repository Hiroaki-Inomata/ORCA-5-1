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
      *    ¿ÇÎÅÊó½·ÀÁµá½ñ     COPY (CPSKT4410TMP.INC)              *
      *
      *  ¥³¥Ô¡¼¶ç½¤ÀµÍúÎò
      * Maj/Min/Rev  ½¤Àµ¼Ô  ÆüÉÕ        ÆâÍÆ
      *  01.00.01    µÈÀî    2008/08/01  Ä¢É¼ÍÍ¼°¤ÎÊÑ¹¹¤ËÈ¼¤¦½¤Àµ
      *  01.00.02    µÈÀî    2008/11/20  ¥·¥¹´É¡Ö2007¡×¸øÈñ¤Þ¤È¤áÂÐ±þ
      *  01.00.03    °æ¾å    2019/02/05  ½¤ÀµÍúÎò¥³¥á¥ó¥Èºï½ü
      ************************************************************
       01  SKT4410TMP-REC.
           03  SKT4410TMP-KEY.
               05  SKT4410TMP-SYUTURYOKU            PIC  9(02).
               05  SKT4410TMP-HKNJANUM              PIC  X(06).
               05  SKT4410TMP-SYUTURYOKU2           PIC  9(01).
               05  SKT4410TMP-ZAITAKU               PIC  X(01).
               05  SKT4410TMP-KYURATE               PIC  9(02).
           03  SKT4410TMP-PAGE-FLG1                 PIC  9(01).
           03  SKT4410TMP-PAGE-FLG2                 PIC  9(01).
           03  SKT4410TMP-PAGE-FLG3                 PIC  9(01).
           03  SKT4410TMP-HKN-AREA  OCCURS  11.
               05  SKT4410TMP-NYUGAI-TBL  OCCURS  2.
                   07  SKT4410TMP-KENSU             PIC  9(05).
                   07  SKT4410TMP-NISSU             PIC  9(05).
                   07  SKT4410TMP-TENSU             PIC  9(09).
                   07  SKT4410TMP-ITBFTN            PIC  9(09).
               05  SKT4410TMP-SKENSU                PIC  9(05).
               05  SKT4410TMP-SNISSU                PIC  9(05).
               05  SKT4410TMP-SKINGK                PIC  9(09).
               05  SKT4410TMP-SFTNGK                PIC  9(09).
           03  SKT4410TMP-KOH-AREA.
               05  SKT4410TMP-KOH-TBL  OCCURS  20.
                   07  SKT4410TMP-KOHNUM              PIC  X(03).
                   07  SKT4410TMP-KOHNAME             PIC  X(10).
                   07  SKT4410TMP-KOH-NYUGAI-TBL OCCURS 2.
                       09  SKT4410TMP-K-KENSU         PIC  9(05).
                       09  SKT4410TMP-K-NISSU         PIC  9(05).
                       09  SKT4410TMP-K-TENSU         PIC  9(09).
                       09  SKT4410TMP-K-ITBFTN        PIC  9(09).
                   07  SKT4410TMP-K-SKENSU            PIC  9(05).
                   07  SKT4410TMP-K-SNISSU            PIC  9(05).
                   07  SKT4410TMP-K-SKINGK            PIC  9(09).
                   07  SKT4410TMP-K-SFTNGK            PIC  9(09).
